pub mod error_report;
pub mod pratt_util;
pub mod tag_util;
#[cfg(test)]
mod test;

use rowan::{Checkpoint, GreenNodeBuilder, SyntaxNode};

use crate::node::InkstoneLang;
use crate::node::SynTag::{self, *};
use crate::Lexer;

use self::error_report::{
    IntoTextRange, ParseError, ParseErrorKind, ParseErrorSignal, ParseResult, Result,
};
use self::pratt_util::{
    infix_binding_power, postfix_binding_power, prefix_binding_power, FUNCTION_CALL_PRECEDENCE,
};

pub type Errors = Vec<ParseError>;

/// Expects a token, or put all following tokens into a node labeled `Error`,
/// until the next token satisfies $recovery.
/// Closes `close_nodes = ?` nodes, and then executes a block
macro_rules! expect_or_recover_with {
    ($self:expr, $expect:expr, $recovery:expr $(, close_nodes = $finish_node_count:expr)? $(, $after:block)?) => {
        match $self.expect($expect) {
            Ok(_) => {},
            Err(_) => {
                $self.recover_error($recovery);
                $(
                    for _ in 0..$finish_node_count {
                        $self.finish_node();
                    }
                )?
                $(
                    $after;
                )?
                return ParseResult::ok_parse_result();
            }
        }
    };
    ($self:expr, $expect:expr, $recovery:expr $(, close_nodes = $finish_node_count:expr)? $(, $after:block)? , no_return) => {
        match $self.expect($expect) {
            Ok(_) => {},
            Err(_) => {
                $self.recover_error($recovery);
                $(
                    for _ in 0..$finish_node_count {
                        $self.finish_node();
                    }
                )?
                $(
                    $after;
                )?
            }
        }
    };
}

/// See [`expect_or_recover_with`], but with a custom parsing expression that returns a
/// [`Result`].
macro_rules! recover_with {
    ($self:expr, $parse:expr $(,recovery = $recovery:expr )? $(, close_nodes =  $finish_node_count:expr)? $(, $after:block)?) => {
        match $parse {
            Ok(t) => t,
            Err(_e) => {
                $($self.recover_error($recovery);)?
                $(
                    for _ in 0..$finish_node_count {
                        $self.finish_node();
                    }
                )?
                $($after;)?
                return ParseResult::ok_parse_result();
            }
        }
    };
    ($self:expr, $parse:expr $(,recovery = $recovery:expr )? $(, close_nodes =  $finish_node_count:expr)? $(, $after:block)?, no_return) => {
        match $parse {
            Ok(t) => t,
            Err(_e) => {
                $($self.recover_error($recovery);)?
                $(
                    for _ in 0..$finish_node_count {
                        $self.finish_node();
                    }
                )?
                $($after;)?
            }
        }
    };
}

/// The main parser that does the job.
pub struct Parser<'src> {
    /// The lexer that does the job.
    lexer: Lexer<'src>,

    /// The real AST builder.
    ///
    /// Its short name is due to the fact that it's called _very_ often.
    b: GreenNodeBuilder<'src>,

    /// List of errors. To be changed.
    errors: Errors,

    /// Stack of synchronization tokens (e.g. `(`/`)`, `begin`/`end`, etc.).
    ///
    /// This stack will be used to perform error recovery.
    sync_token_stack: Vec<SynTag>,

    #[cfg(debug_assertions)]
    /// A node tree for debugging use
    debug_node_tree: Vec<SynTag>,
}

impl<'src> Parser<'src> {
    pub fn new(s: &'src str) -> Self {
        Parser {
            lexer: Lexer::new(s),
            b: GreenNodeBuilder::new(),
            errors: Vec::new(),
            sync_token_stack: Vec::new(),
            #[cfg(debug_assertions)]
            debug_node_tree: Vec::new(),
        }
    }

    pub fn parse(&mut self) {
        self.parse_root()
    }

    pub fn finish(self) -> (SyntaxNode<InkstoneLang>, Errors) {
        (SyntaxNode::new_root(self.b.finish()), self.errors)
    }

    fn start_node(&mut self, tag: SynTag) {
        #[cfg(debug_assertions)]
        {
            tracing::debug!("> Start node.  {:?} ++ {:?}", self.debug_node_tree, tag);
            self.debug_node_tree.push(tag);
        }
        self.b.start_node(tag.into());
    }

    fn start_node_at(&mut self, checkpoint: Checkpoint, tag: SynTag) {
        #[cfg(debug_assertions)]
        {
            tracing::debug!(
                "> Start node.  {:?} ++ {:?} <: {:?}",
                self.debug_node_tree,
                tag,
                checkpoint
            );
            self.debug_node_tree.push(tag);
        }
        self.b.start_node_at(checkpoint, tag.into());
    }

    fn finish_node(&mut self) {
        #[cfg(debug_assertions)]
        {
            let tag = self.debug_node_tree.pop();
            tracing::debug!("< Finish node. {:?} :: {:?}", self.debug_node_tree, tag,);
        }
        self.b.finish_node();
    }

    fn emit_error(&mut self, err: ParseError) {
        tracing::info!(
            "! Emitting error {:?}; current peek: {:?}",
            err,
            self.peek()
        );
        self.errors.push(err)
    }

    fn recover_error(&mut self, predicate: impl Fn(SynTag) -> bool) {
        tracing::debug!("# Begin error recovery");
        self.start_node(Error);
        self.eat_if(|t| !predicate(t));
        self.finish_node();
        tracing::debug!("#!Recovery finished");
    }

    fn eat_if<F: FnOnce(SynTag) -> bool>(&mut self, f: F) -> Option<SynTag> {
        let tok = self.lexer.peek().unwrap_or(Eof);
        debug_assert!(tok.is_token());
        if f(tok) {
            self.lexer.next();
            tracing::debug!(": Input token: {:?}", tok);
            if tok != Eof {
                self.b.token(tok.into(), self.lexer.slice());
            }
            Some(tok)
        } else {
            None
        }
    }

    fn eat(&mut self) -> SynTag {
        self.eat_if(|_| true).unwrap_or(Eof)
    }

    fn try_eat_token(&mut self, s: SynTag) -> bool {
        self.eat_if(|t| t == s).is_some()
    }

    #[must_use = "Do error recovery if expectation failed"]
    fn expect(&mut self, s: SynTag) -> Result<()> {
        if !self.try_eat_token(s) {
            let got = self.peek();
            let span = self.lexer.peek_span().into_text_range();
            self.emit_error(ParseError::error(
                span,
                ParseErrorKind::Expected {
                    expected: s,
                    got: Some(got),
                },
            ));
            Err(ParseErrorSignal)
        } else {
            Ok(())
        }
    }

    fn peek(&mut self) -> SynTag {
        self.lexer.peek().unwrap_or(Eof)
    }

    fn eat_whitespace(&mut self) {
        while self.eat_if(|x| x == WS).is_some() {}
    }

    fn eat_whitespace_or_line_feeds(&mut self) {
        while self.eat_if(|x| x == WS || x == LF).is_some() {}
    }

    fn eat_whitespace_in_parenthesis(&mut self, in_parenthesis: bool) {
        while self
            .eat_if(|x| x == WS || (in_parenthesis && x == LF))
            .is_some()
        {}
    }
}

/// The concrete parsing implementations.
///
/// # Note
///
/// - Parsing methods should not take care of any whitespace or line feed around
///   it, unless necessary.
/// - Whitespace tokens around a node should be placed **outside** that node.
impl<'src> Parser<'src> {
    fn parse_root(&mut self) {
        self.start_node(Root);
        self.eat_whitespace_or_line_feeds();

        self.parse_block_scope(|t| t == Eof);

        self.finish_node();
    }

    fn parse_func_def(&mut self, checkpoint: Checkpoint) {
        // def name param1 params = body
        self.start_node_at(checkpoint, FuncDef);
        expect_or_recover_with!(self, DefKw, |_| true, close_nodes = 1);
        self.eat_whitespace_or_line_feeds();

        self.start_node(Name);
        expect_or_recover_with!(self, Ident, |t| t == Assign, close_nodes = 2);
        self.finish_node();

        self.eat_whitespace_or_line_feeds();
        self.parse_param_list();

        self.eat_whitespace_or_line_feeds();
        expect_or_recover_with!(
            self,
            Assign,
            |t| t.is_stmt_parsing_sync_token(),
            close_nodes = 1
        );
        self.eat_whitespace_or_line_feeds();

        recover_with!(
            self,
            self.parse_expr(false),
            recovery = |t| t.is_stmt_parsing_sync_token(),
            close_nodes = 1
        );

        self.eat_whitespace();
        self.parse_stmt_end();
        self.finish_node();
    }

    fn parse_block_scope(&mut self, is_end_token: impl Fn(SynTag) -> bool) {
        self.start_node(BlockScope);
        self.eat_whitespace_or_line_feeds();

        while !(is_end_token(self.peek()) || self.peek() == Eof) {
            self.parse_stmt();
            self.eat_whitespace_or_line_feeds();
        }
        self.finish_node();
    }

    fn parse_block(&mut self) {
        // begin
        //   ...stmt
        // end
        self.start_node(Block);
        expect_or_recover_with!(self, BeginKw, |_| true, { self.b.finish_node() });

        self.sync_token_stack.push(BeginKw);
        self.eat_whitespace_or_line_feeds();

        self.parse_block_scope(|t| t == EndKw);

        self.eat_whitespace_or_line_feeds();

        expect_or_recover_with!(self, EndKw, |_| true, close_nodes = 1);
        self.sync_token_stack.pop();

        self.finish_node();
    }

    fn parse_let_stmt(&mut self, checkpoint: Checkpoint) {
        // let x = expr

        self.start_node_at(checkpoint, LetStmt);
        expect_or_recover_with!(self, LetKw, |_| true, close_nodes = 1);
        self.eat_whitespace_or_line_feeds();

        self.parse_binding_expr();

        self.eat_whitespace_or_line_feeds();
        expect_or_recover_with!(
            self,
            Assign,
            SynTag::is_stmt_parsing_sync_token,
            close_nodes = 1
        );
        self.eat_whitespace_or_line_feeds();

        recover_with!(
            self,
            self.parse_expr(false),
            recovery = SynTag::is_stmt_parsing_sync_token,
            close_nodes = 1
        );

        self.eat_whitespace();
        self.parse_stmt_end();
        self.finish_node();
    }

    fn parse_module_stmt(&mut self, checkpoint: Checkpoint) {
        self.start_node_at(checkpoint, SynTag::ModuleDef);
        expect_or_recover_with!(self, ModKw, |_| true, close_nodes = 1);
        self.eat_whitespace_or_line_feeds();

        self.start_node(Name);
        expect_or_recover_with!(self, Ident, |t| t == EndKw, close_nodes = 2);
        self.finish_node();
        self.eat_whitespace_or_line_feeds();

        self.parse_block_scope(|t| t == SynTag::EndKw);
        expect_or_recover_with!(self, EndKw, |_| true, close_nodes = 1);
        self.finish_node();
    }

    fn parse_stmt_end(&mut self) {
        if self.eat_if(|t| matches!(t, Semicolon | LF | Eof)).is_none() {
            let peek = self.peek();
            let span = self.lexer.peek_span().into_text_range();
            self.emit_error(ParseError::error(span, ParseErrorKind::Unexpected(peek)));
            self.recover_error(|t| matches!(t, Semicolon | LF | Eof));
            self.eat_if(|t| t == Semicolon || t == LF);
        }
    }

    fn parse_optional_visibility(&mut self) {
        // pub?
        let checkpoint = self.b.checkpoint();
        if self.peek() == SynTag::PubKw {
            self.start_node_at(checkpoint, SynTag::Visibility);
            self.start_node(SynTag::Pub);
            self.eat();
            self.finish_node();
        }
        self.finish_node();
    }

    fn parse_stmt_with_optional_visibility(&mut self) {
        let checkpoint = self.b.checkpoint();
        self.parse_optional_visibility();
        self.eat_whitespace();
        match self.peek() {
            DefKw => self.parse_func_def(checkpoint),
            LetKw => self.parse_let_stmt(checkpoint),
            ModKw => self.parse_module_stmt(checkpoint),
            _ => {
                let span = self.lexer.peek_span().into_text_range();
                self.emit_error(ParseError::error(span, ParseErrorKind::ExpectStmt));
                self.recover_error(SynTag::is_stmt_parsing_sync_token);
            }
        }
    }

    fn parse_stmt(&mut self) {
        match self.peek() {
            t if t.can_start_expr() => {
                self.start_node(ExprStmt);
                recover_with!(
                    self,
                    self.parse_expr(false),
                    recovery = SynTag::is_stmt_parsing_sync_token,
                    close_nodes = 1
                );
                self.eat_whitespace();
                self.parse_stmt_end();
                self.finish_node();
            }
            DefKw | LetKw | PubKw | ModKw => self.parse_stmt_with_optional_visibility(),
            _ => {
                let span = self.lexer.peek_span().into_text_range();
                self.emit_error(ParseError::error(span, ParseErrorKind::ExpectStmt));
                self.recover_error(SynTag::is_stmt_parsing_sync_token);
                // self.eat();
            }
        }
    }

    fn parse_expr(&mut self, in_parenthesis: bool) -> Result<()> {
        debug_assert!(
            self.peek().can_start_expr(),
            "Must start with some token that can start an expression, got {:?} @ {:?}",
            self.peek(),
            self.lexer.inner.remainder()
        );

        self.parse_expr_pratt(0, in_parenthesis, false)
    }

    fn parse_expr_pratt(
        &mut self,
        start_precedence: i32,
        in_parenthesis: bool,
        in_function_call: bool,
    ) -> Result<()> {
        // This method uses pratt parsing

        // This checkpoint wraps the whole left-hand side
        let start = self.b.checkpoint();

        {
            // parse prefix expr
            let tok = self.peek();
            if let Some(bp) = prefix_binding_power(tok) {
                self.start_node(UnaryExpr);

                self.start_node(UnaryOp);
                self.eat();
                self.finish_node();

                self.eat_whitespace_or_line_feeds();
                recover_with!(
                    self,
                    self.parse_expr_pratt(bp, in_parenthesis, in_function_call),
                    close_nodes = 1
                );
                self.finish_node();
            } else {
                self.eat_whitespace_in_parenthesis(in_parenthesis);
                self.parse_primary_expr()?;
            }
        }
        self.eat_whitespace_in_parenthesis(in_parenthesis);

        loop {
            self.eat_whitespace_in_parenthesis(in_parenthesis);
            if let Some(bp) = postfix_binding_power(self.peek()) {
                if bp < start_precedence {
                    break;
                }

                self.start_node_at(start, UnaryExpr);

                self.start_node(UnaryOp);
                self.eat();
                self.finish_node();

                self.finish_node();
            } else if let Some((lbp, rbp)) =
                infix_binding_power(self.peek()).map(|x| x.binding_power())
            {
                if lbp < start_precedence {
                    break;
                }
                if self.peek() == LBracket {
                    // subscript expr
                    self.start_node_at(start, SubscriptExpr);
                    self.eat();
                    self.eat_whitespace_or_line_feeds();
                    match self.parse_expr_pratt(rbp, true, false) {
                        Ok(_) => {}
                        Err(_) => {
                            self.recover_error(|t| t == RBracket);
                        }
                    };
                    self.eat_whitespace_in_parenthesis(true);
                    expect_or_recover_with!(self, RBracket, |_| true, close_nodes = 1);
                    self.finish_node();
                } else if self.peek() == Dot {
                    // dot expr only accepts identifiers
                    self.start_node_at(start, DotExpr);

                    while self.try_eat_token(Dot) {
                        self.eat_whitespace_or_line_feeds();
                        expect_or_recover_with!(
                            self,
                            Ident,
                            |t| t == Dot || t.is_expr_parsing_sync_token(),
                            no_return
                        );
                        self.eat_whitespace_in_parenthesis(in_parenthesis);
                    }
                } else if self.peek() == Assign {
                    self.start_node_at(start, AssignExpr);

                    self.eat();
                    self.eat_whitespace_or_line_feeds();

                    recover_with!(
                        self,
                        self.parse_expr_pratt(rbp, in_parenthesis, in_function_call),
                        close_nodes = 1
                    );
                } else {
                    self.start_node_at(start, BinaryExpr);

                    self.start_node(BinaryOp);
                    self.eat();
                    self.finish_node();

                    self.eat_whitespace_or_line_feeds();
                    recover_with!(
                        self,
                        self.parse_expr_pratt(rbp, in_parenthesis, in_function_call),
                        close_nodes = 1
                    );
                }
                self.finish_node();
            } else if self.peek().can_start_expr() && !in_function_call {
                if FUNCTION_CALL_PRECEDENCE < start_precedence {
                    break;
                }
                // function call
                self.start_node_at(start, FunctionCallExpr);
                self.eat_whitespace_in_parenthesis(in_parenthesis);

                // parse function params
                while self.peek().can_start_expr()
                    && (prefix_binding_power(self.peek())
                        .map_or(true, |p| p > FUNCTION_CALL_PRECEDENCE))
                {
                    recover_with!(
                        self,
                        self.parse_expr_pratt(FUNCTION_CALL_PRECEDENCE, in_parenthesis, true),
                        close_nodes = 1
                    );
                    self.eat_whitespace_in_parenthesis(in_parenthesis)
                }

                self.finish_node();
            } else {
                break;
            }
        }

        Ok(())
    }

    fn parse_primary_expr(&mut self) -> Result<()> {
        match self.peek() {
            BeginKw => {
                self.start_node(BlockExpr);
                self.parse_block();
                self.finish_node();
            }
            Int | Float | StringLiteral | Symbol | TrueKw | FalseKw => {
                self.start_node(LiteralExpr);
                self.eat();
                self.finish_node();
            }
            LBracket => {
                self.parse_array_literal();
            }
            LBrace => {
                self.parse_object_literal();
            }
            Ident => {
                self.parse_ident()?;
            }
            LParen => {
                self.parse_paren_expr_or_tuple();
            }
            Backslash => {
                self.parse_lambda();
            }
            IfKw => {
                self.parse_if_expr();
            }
            WhileKw => {
                self.parse_while_expr();
            }
            ForKw => {
                self.parse_for_expr();
            }
            MatchKw => {
                todo!("parse pattern matching")
            }
            BreakKw => {
                self.parse_break_expr();
            }
            ReturnKw => {
                self.parse_return_expr();
            }
            got => {
                let span = self.lexer.peek_span().into_text_range();
                self.emit_error(ParseError::error(
                    span,
                    ParseErrorKind::ExpectedString {
                        expected: "An expression".into(),
                        got: got.into(),
                    },
                ));
                return Err(ParseErrorSignal);
            }
        }
        Ok(())
    }

    fn parse_if_expr(&mut self) {
        // if expr; if_block; else else_block; end
        //
        // -or-
        //
        // if expr
        //   if_block
        // else
        //   else_block
        // end

        self.start_node(IfExpr);
        expect_or_recover_with!(self, IfKw, SynTag::is_block_parsing_sync_token, {
            self.b.finish_node()
        });
        self.eat_whitespace_or_line_feeds();

        self.parse_if_branch();

        self.eat_whitespace_or_line_feeds();

        loop {
            match self.peek() {
                ElseKw => {
                    // else branch
                    self.eat();
                    self.eat_whitespace_or_line_feeds();

                    if self.try_eat_token(IfKw) {
                        self.eat_whitespace_or_line_feeds();
                        self.parse_if_branch();
                    } else {
                        self.eat_whitespace_or_line_feeds();
                        self.start_node(IfBranch);
                        self.parse_block_scope(|t| t == EndKw || t == ElseKw);
                        self.finish_node();
                    }
                    self.eat_whitespace_or_line_feeds();
                }
                EndKw => {
                    self.eat();
                    break;
                }
                others => {
                    let span = self.lexer.peek_span().into_text_range();
                    self.emit_error(ParseError::error(
                        span,
                        ParseErrorKind::ExpectedString {
                            expected: "`else` or `end`".into(),
                            got: others.into(),
                        },
                    ));
                    break;
                }
            }
        }

        self.finish_node();
    }

    fn parse_if_branch(&mut self) {
        self.start_node(IfBranch);
        {
            self.start_node(Condition);
            match self.parse_expr(false) {
                Ok(_) => {
                    self.finish_node();
                }
                Err(_) => {
                    self.recover_error(|t| t.is_stmt_parsing_sync_token() || t == ElseKw);
                    if matches!(self.peek(), ElseKw | EndKw) {
                        self.finish_node();
                        return;
                    }
                }
            }
            self.eat_whitespace();
            self.parse_stmt_end();
        }
        self.eat_whitespace_or_line_feeds();
        self.parse_block_scope(|t| t == EndKw || t == ElseKw);
        self.finish_node();
    }

    fn parse_while_expr(&mut self) {
        // while cond
        //   body
        // end

        self.start_node(WhileLoopExpr);
        expect_or_recover_with!(self, WhileKw, |_| true, close_nodes = 1);
        self.eat_whitespace_or_line_feeds();

        self.start_node(Condition);
        match self.parse_expr(false) {
            Ok(_) => {
                self.finish_node();
            }
            Err(_) => {
                self.recover_error(|t| t.is_stmt_parsing_sync_token());
                self.finish_node();
                if matches!(self.peek(), EndKw) {
                    self.finish_node();
                    return;
                }
            }
        }

        self.eat_whitespace();
        self.parse_stmt_end();

        self.parse_block_scope(|t| t == EndKw);

        self.eat_whitespace_or_line_feeds();
        expect_or_recover_with!(self, EndKw, |_| true, close_nodes = 1);
        self.finish_node();
    }

    fn parse_for_expr(&mut self) {
        // for bind in val
        //   body
        // end

        self.start_node(ForLoopExpr);
        expect_or_recover_with!(
            self,
            ForKw,
            SynTag::is_block_parsing_sync_token,
            close_nodes = 1
        );
        self.eat_whitespace_or_line_feeds();

        self.start_node(Condition);
        self.parse_binding_expr();

        self.eat_whitespace();
        expect_or_recover_with!(
            self,
            InKw,
            SynTag::is_block_parsing_sync_token,
            close_nodes = 2
        );
        self.eat_whitespace_or_line_feeds();

        recover_with!(
            self,
            self.parse_expr(false),
            recovery = SynTag::is_stmt_parsing_sync_token,
            no_return
        );
        self.finish_node();

        self.eat_whitespace();
        self.parse_stmt_end();

        self.parse_block_scope(|t| t == EndKw);

        self.eat_whitespace_or_line_feeds();
        expect_or_recover_with!(self, EndKw, |_| true, close_nodes = 2);
        self.finish_node();
    }

    fn parse_binding_expr(&mut self) {
        self.start_node(Binding);
        match self.expect(Ident) {
            Ok(_) => {}
            Err(_) => {
                self.start_node(Error);
                self.eat();
                self.finish_node();
            }
        };
        self.finish_node();
    }

    fn parse_break_expr(&mut self) {
        self.start_node(BreakExpr);
        expect_or_recover_with!(self, BreakKw, |_| true, close_nodes = 1);
        self.eat_whitespace();
        if self.peek().can_start_expr() {
            recover_with!(
                self,
                self.parse_expr(false),
                recovery = SynTag::is_expr_parsing_sync_token,
                close_nodes = 1
            );
        }
        self.finish_node();
    }

    fn parse_return_expr(&mut self) {
        self.start_node(ReturnExpr);
        expect_or_recover_with!(self, ReturnKw, |_| true, close_nodes = 1);
        self.eat_whitespace();
        if self.peek().can_start_expr() {
            recover_with!(
                self,
                self.parse_expr(false),
                recovery = SynTag::is_expr_parsing_sync_token,
                close_nodes = 1
            );
        }
        self.finish_node();
    }

    fn parse_ident(&mut self) -> Result<()> {
        let ns_start = self.b.checkpoint();
        debug_assert_eq!(
            self.peek(),
            Ident,
            "name or namespace must start with an ident"
        );
        self.eat();

        self.start_node_at(ns_start, IdentExpr);

        self.finish_node();
        Ok(())
    }

    fn parse_paren_expr_or_tuple(&mut self) {
        let paren_start = self.b.checkpoint();
        debug_assert_eq!(self.peek(), LParen, "Must start with left parenthesis");
        self.eat();

        self.eat_whitespace_or_line_feeds();

        if self.try_eat_token(RParen) {
            // Empty tuple
            self.start_node_at(paren_start, TupleLiteralExpr);
            self.finish_node();
            return;
        }

        recover_with!(
            self,
            self.parse_expr(true),
            recovery = SynTag::is_expr_parsing_sync_token,
            no_return
        );
        self.eat_whitespace_or_line_feeds();

        if self.peek() == Comma {
            self.start_node_at(paren_start, TupleLiteralExpr);
            while self.try_eat_token(Comma) {
                self.eat_whitespace_or_line_feeds();
                if self.peek() == RParen {
                    break;
                }
                recover_with!(
                    self,
                    self.parse_expr(true),
                    recovery = SynTag::is_expr_parsing_sync_token,
                    no_return
                );
                self.eat_whitespace_or_line_feeds();
            }
            expect_or_recover_with!(self, RParen, |_| false, close_nodes = 1);
        } else if self.try_eat_token(RParen) {
            self.start_node_at(paren_start, ParenExpr);
        } else {
            let peek = self.peek();
            let span = self.lexer.peek_span().into_text_range();
            self.emit_error(ParseError::error(
                span,
                ParseErrorKind::ExpectedString {
                    expected: "Comma or Right Parenthesis".into(),
                    got: peek.into(),
                },
            ))
        }
        self.finish_node();
    }

    fn parse_array_literal(&mut self) {
        self.start_node(ArrayLiteralExpr);
        expect_or_recover_with!(self, LBracket, |_| true, close_nodes = 1);
        self.eat_whitespace_or_line_feeds();

        if self.peek().can_start_expr() {
            recover_with!(
                self,
                self.parse_expr(true),
                recovery = SynTag::is_expr_parsing_sync_token,
                no_return
            );
            self.eat_whitespace_or_line_feeds();
            while self.try_eat_token(Comma) {
                self.eat_whitespace_or_line_feeds();
                if !self.peek().can_start_expr() {
                    break;
                }
                recover_with!(
                    self,
                    self.parse_expr(true),
                    recovery = SynTag::is_expr_parsing_sync_token,
                    no_return
                );
                self.eat_whitespace_or_line_feeds();
            }
        }

        self.eat_whitespace_or_line_feeds();
        expect_or_recover_with!(
            self,
            RBracket,
            |t| t == RBracket || t == Eof,
            close_nodes = 1,
            {
                self.eat();
            }
        );
        self.finish_node();
    }

    fn parse_key_value_pair(&mut self, in_parenthesis: bool) {
        // key: value
        // -or-
        // key
        self.start_node(KeyValuePair);

        self.start_node(Name);
        if self
            .eat_if(|t| t == StringLiteral || t == Ident || t == Symbol)
            .is_none()
        {
            let got = self.peek().into();
            let span = self.lexer.peek_span().into_text_range();
            self.emit_error(ParseError::error(
                span,
                ParseErrorKind::ExpectedString {
                    expected: "Identifier, String literal or symbol".into(),
                    got,
                },
            ));
            self.recover_error(|t| t == Colon || t.is_expr_parsing_sync_token());
            if self.peek() != Colon {
                self.finish_node();
                self.finish_node();
                return;
            }
        }
        self.finish_node();

        self.eat_whitespace_in_parenthesis(in_parenthesis);
        expect_or_recover_with!(
            self,
            Colon,
            SynTag::is_expr_parsing_sync_token,
            close_nodes = 1
        );
        self.eat_whitespace_in_parenthesis(in_parenthesis);

        recover_with!(
            self,
            self.parse_expr(in_parenthesis),
            recovery = SynTag::is_expr_parsing_sync_token,
            close_nodes = 1
        );

        self.finish_node();
    }

    fn parse_object_literal(&mut self) {
        // {
        //   key: value, ...
        // }
        self.start_node(ObjectLiteralExpr);
        expect_or_recover_with!(self, LBrace, |_| true, close_nodes = 1);

        self.eat_whitespace_or_line_feeds();

        if matches!(self.peek(), Ident | StringLiteral | Symbol) {
            self.parse_key_value_pair(true);
            self.eat_whitespace_or_line_feeds();
            while self.peek() == Comma {
                self.eat();

                self.eat_whitespace_or_line_feeds();
                if !(matches!(self.peek(), Ident | StringLiteral | Symbol)) {
                    break;
                }
                self.parse_key_value_pair(true);
                self.eat_whitespace_or_line_feeds();
            }
        }

        self.eat_whitespace_or_line_feeds();

        expect_or_recover_with!(
            self,
            RBrace,
            |t| t == RBrace || t == Eof,
            close_nodes = 1,
            {
                self.eat();
            }
        );
        self.finish_node();
    }

    fn parse_lambda(&mut self) {
        // \x -> body
        self.start_node(LambdaExpr);
        expect_or_recover_with!(self, Backslash, |_| true, close_nodes = 1);

        self.parse_param_list();

        expect_or_recover_with!(
            self,
            Arrow,
            |t| t.is_expr_parsing_sync_token(),
            close_nodes = 1
        );
        self.eat_whitespace_or_line_feeds();

        self.start_node(FuncBody);
        recover_with!(
            self,
            self.parse_expr(false),
            recovery = |t| t.is_expr_parsing_sync_token(),
            close_nodes = 2
        );
        self.finish_node();
        self.finish_node();
    }

    fn parse_param_list(&mut self) {
        self.start_node(FuncParamList);
        while self.peek() == Ident {
            self.start_node(FuncParam);
            self.eat();
            self.finish_node();
            self.eat_whitespace_or_line_feeds();
        }
        self.finish_node();
    }
}
