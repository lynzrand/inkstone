mod error_report;
mod pratt_util;
mod tag_util;
#[cfg(test)]
mod test;

use rowan::{GreenNode, GreenNodeBuilder, SyntaxNode};

use crate::node::InkstoneLang;
use crate::node::SynTag::{self, *};
use crate::Lexer;

use self::pratt_util::{
    infix_binding_power, postfix_binding_power, prefix_binding_power, FUNCTION_CALL_PRECEDENCE,
};

pub type Errors = Vec<String>;

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
}

impl<'src> Parser<'src> {
    pub fn new(s: &'src str) -> Self {
        Parser {
            lexer: Lexer::new(s),
            b: GreenNodeBuilder::new(),
            errors: Vec::new(),
            sync_token_stack: Vec::new(),
        }
    }

    pub fn parse(&mut self) {
        self.parse_root()
    }

    pub fn finish(self) -> (SyntaxNode<InkstoneLang>, Errors) {
        (SyntaxNode::new_root(self.b.finish()), self.errors)
    }

    fn log_err(&mut self, err: String) {
        self.errors.push(err)
    }

    fn eat_if<F: FnOnce(SynTag) -> bool>(&mut self, f: F) -> Option<SynTag> {
        let tok = self.lexer.next()?;
        debug_assert!(tok.is_token());
        if f(tok) {
            self.b.token(tok.into(), self.lexer.slice());
            Some(tok)
        } else {
            self.lexer.backtrack_front(tok);
            None
        }
    }

    fn eat(&mut self) -> SynTag {
        self.eat_if(|_| true).unwrap_or(Eof)
    }

    fn try_eat_token(&mut self, s: SynTag) -> bool {
        self.eat_if(|t| t == s).is_some()
    }

    fn expect(&mut self, s: SynTag) -> bool {
        let res = self.try_eat_token(s);
        if !res {
            panic!("Token mismatch! expected: {:?}, got: {:?}", s, self.peek());
        }
        res
    }

    fn peek(&mut self) -> SynTag {
        self.lexer.peek().unwrap_or(Eof)
    }

    fn peek_is(&mut self, s: SynTag) -> bool {
        self.peek_if(|x| x == s)
    }

    fn peek_if<F: FnOnce(SynTag) -> bool>(&mut self, f: F) -> bool {
        self.lexer.peek().map_or(false, f)
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
        self.b.start_node(Root.into());
        self.eat_whitespace_or_line_feeds();

        self.parse_block_scope();

        self.b.finish_node();
    }

    fn parse_func_def(&mut self) {
        // def name param1 params = body
        self.b.start_node(FuncDef.into());
        self.expect(DefKw);
        self.eat_whitespace_or_line_feeds();

        self.b.start_node(Name.into());
        self.expect(Ident);
        self.b.finish_node();

        self.eat_whitespace_or_line_feeds();
        self.parse_param_list();

        self.eat_whitespace_or_line_feeds();
        self.expect(Assign);
        self.eat_whitespace_or_line_feeds();

        self.parse_expr(false);

        self.eat_whitespace();
        self.parse_stmt_end();
        self.b.finish_node();
    }

    fn parse_block_scope(&mut self) {
        self.b.start_node(BlockScope.into());
        self.eat_whitespace_or_line_feeds();

        while self.peek_if(|t| t.can_start_stmt()) {
            self.parse_stmt();
            self.eat_whitespace_or_line_feeds();
        }
        self.b.finish_node();

        self.eat_whitespace_or_line_feeds();
    }

    fn parse_block(&mut self) {
        // begin
        //   ...stmt
        // end
        self.b.start_node(Block.into());
        self.expect(BeginKw);

        self.sync_token_stack.push(BeginKw);
        self.eat_whitespace_or_line_feeds();

        self.parse_block_scope();

        self.eat_whitespace_or_line_feeds();

        self.eat();
        self.sync_token_stack.pop();

        self.b.finish_node();
        self.eat_whitespace_or_line_feeds();
    }

    fn parse_let_stmt(&mut self) {
        // let x = expr

        self.b.start_node(LetStmt.into());
        self.expect(LetKw);
        self.eat_whitespace_or_line_feeds();

        self.b.start_node(Name.into());
        self.expect(Ident);
        self.b.finish_node();

        self.eat_whitespace_or_line_feeds();
        self.expect(Assign);
        self.eat_whitespace_or_line_feeds();

        self.parse_expr(false);

        self.eat_whitespace();
        self.parse_stmt_end();
        self.b.finish_node();
    }

    fn parse_stmt_end(&mut self) {
        self.eat_if(|t| t == LF || t == Semicolon);
    }

    fn parse_stmt(&mut self) {
        match self.peek() {
            t if t.can_start_expr() => {
                self.b.start_node(ExprStmt.into());
                self.parse_expr(false);
                self.eat_whitespace();
                self.parse_stmt_end();
                self.b.finish_node();
            }
            DefKw => {
                self.parse_func_def();
            }
            LetKw => {
                self.parse_let_stmt();
            }
            _ => {
                panic!("unknown stmt, got {:?}", self.peek());
            }
        }
    }

    fn parse_expr(&mut self, in_parenthesis: bool) {
        debug_assert!(
            self.peek().can_start_expr(),
            "Must start with some token that can start an expression, got {:?} @ {:?}",
            self.peek(),
            self.lexer.inner.remainder()
        );

        self.parse_expr_pratt(0, in_parenthesis, false);
    }

    fn parse_expr_pratt(
        &mut self,
        start_precedence: i32,
        in_parenthesis: bool,
        in_function_call: bool,
    ) {
        // This method uses pratt parsing

        // This checkpoint wraps the whole left-hand side
        let start = self.b.checkpoint();

        {
            // parse prefix expr
            let tok = self.peek();
            if let Some(bp) = prefix_binding_power(tok) {
                self.b.start_node(UnaryExpr.into());
                self.eat();
                self.eat_whitespace_or_line_feeds();
                self.parse_expr_pratt(bp, in_parenthesis, in_function_call);
                self.b.finish_node();
            } else {
                self.eat_whitespace_in_parenthesis(in_parenthesis);
                self.parse_primary_expr();
            }
        }
        self.eat_whitespace_in_parenthesis(in_parenthesis);

        loop {
            self.eat_whitespace_in_parenthesis(in_parenthesis);
            if let Some(bp) = postfix_binding_power(self.peek()) {
                if bp < start_precedence {
                    break;
                }

                self.b.start_node_at(start, UnaryExpr.into());
                self.eat();

                self.b.finish_node();
            } else if let Some((lbp, rbp)) =
                infix_binding_power(self.peek()).map(|x| x.binding_power())
            {
                if lbp < start_precedence {
                    break;
                }
                if self.peek() == LBracket {
                    // subscript expr
                    self.b.start_node_at(start, SubscriptExpr.into());
                    self.eat();
                    self.eat_whitespace_or_line_feeds();
                    self.parse_expr_pratt(rbp, true, false);
                    self.eat_whitespace_in_parenthesis(true);
                    self.expect(RBracket);
                } else if self.peek() == Dot {
                    // dot expr only accepts identifiers
                    self.b.start_node_at(start, DotExpr.into());

                    while self.try_eat_token(Dot) {
                        self.eat_whitespace_or_line_feeds();
                        if !self.expect(Ident) {
                            panic!("todo expect ident")
                        }
                        self.eat_whitespace_in_parenthesis(in_parenthesis);
                    }
                } else {
                    self.b.start_node_at(start, BinaryExpr.into());
                    self.eat();
                    self.eat_whitespace_or_line_feeds();
                    self.parse_expr_pratt(rbp, in_parenthesis, in_function_call);
                }
                self.b.finish_node();
            } else if self.peek().can_start_expr() && !in_function_call {
                if FUNCTION_CALL_PRECEDENCE < start_precedence {
                    break;
                }
                // function call
                self.b.start_node_at(start, FunctionCallExpr.into());
                self.eat_whitespace_in_parenthesis(in_parenthesis);

                // parse function params
                while self.peek().can_start_expr() {
                    self.parse_expr_pratt(FUNCTION_CALL_PRECEDENCE, in_parenthesis, true);
                    self.eat_whitespace_in_parenthesis(in_parenthesis)
                }

                self.b.finish_node();
            } else {
                break;
            }
        }
    }

    fn parse_primary_expr(&mut self) {
        match self.peek() {
            BeginKw => {
                self.b.start_node(BlockExpr.into());
                self.parse_block();
                self.b.finish_node();
            }
            Int | Float | StringLiteral | Symbol => {
                self.b.start_node(LiteralExpr.into());
                self.eat();
                self.b.finish_node();
            }
            LBracket => {
                self.parse_array_literal();
            }
            LBrace => {
                self.parse_object_literal();
            }
            Ident => {
                self.parse_name_or_namespace();
            }
            LParen => {
                self.parse_paren_expr_or_tuple();
            }
            Backslash => {
                self.parse_lambda();
            }
            _ => {
                todo!("got {:?}: `{}`", self.peek(), self.lexer.inner.remainder())
            }
        }
    }

    fn parse_name_or_namespace(&mut self) {
        let ns_start = self.b.checkpoint();
        debug_assert_eq!(
            self.peek(),
            Ident,
            "name or namespace must start with an ident"
        );
        self.eat();

        if self.peek_is(DoubleColon) {
            self.b.start_node_at(ns_start, Namespace.into());
            while self.try_eat_token(DoubleColon) {
                self.expect(Ident);
                // TODO: error reduction
            }
            self.b.finish_node();
        } else {
            self.b.start_node_at(ns_start, VarExpr.into());
            self.b.finish_node();
        }
    }

    fn parse_paren_expr_or_tuple(&mut self) {
        let paren_start = self.b.checkpoint();
        debug_assert_eq!(self.peek(), LParen, "Must start with left parenthesis");
        self.eat();

        self.parse_expr(true);
        self.eat_whitespace_or_line_feeds();

        if self.peek_is(Comma) {
            self.b.start_node_at(paren_start, TupleLiteralExpr.into());
            while self.try_eat_token(Comma) {
                if self.try_eat_token(RParen) {
                    break;
                }
                self.parse_expr(true);
                self.eat_whitespace_or_line_feeds();
            }
            self.b.finish_node();
        } else if self.try_eat_token(RParen) {
            self.b.start_node_at(paren_start, ParenExpr.into());
            self.b.finish_node();
        } else {
            todo!(
                "Error: expect comma or parenthesis, got {:?}: `{}`",
                self.peek(),
                self.lexer.inner.remainder()
            )
        }
    }

    fn parse_array_literal(&mut self) {
        self.b.start_node(ArrayLiteralExpr.into());
        self.expect(LBracket);
        self.eat_whitespace_or_line_feeds();

        if self.peek_if(|t| t.can_start_expr()) {
            self.parse_expr(true);
            self.eat_whitespace_or_line_feeds();
            while self.peek_is(Comma) {
                self.expect(Comma);
                self.eat_whitespace_or_line_feeds();
                self.parse_expr(true);
                self.eat_whitespace_or_line_feeds();
            }
            self.try_eat_token(Comma);
        }

        self.eat_whitespace_or_line_feeds();
        self.expect(RBracket);
        self.b.finish_node();
    }

    fn parse_key_value_pair(&mut self, in_parenthesis: bool) {
        // key: value
        self.b.start_node(KeyValuePair.into());

        self.b.start_node(Name.into());
        if self
            .eat_if(|t| t == StringLiteral || t == Ident || t == Symbol)
            .is_none()
        {
            panic!("not key: {:?}", self.peek());
        }
        self.b.finish_node();

        self.eat_whitespace_in_parenthesis(in_parenthesis);
        self.expect(Colon);
        self.eat_whitespace_in_parenthesis(in_parenthesis);

        self.parse_expr(in_parenthesis);

        self.b.finish_node();
    }

    fn parse_object_literal(&mut self) {
        // {
        //   key: value, ...
        // }
        self.b.start_node(ObjectLiteralExpr.into());
        self.expect(LBrace);

        self.eat_whitespace_or_line_feeds();

        if self.peek_is(Ident) || self.peek_is(StringLiteral) || self.peek_is(Symbol) {
            self.parse_key_value_pair(true);
            self.eat_whitespace_or_line_feeds();
            while self.peek_is(Comma) {
                self.expect(Comma);
                self.eat_whitespace_or_line_feeds();
                if !(self.peek_is(Ident) || self.peek_is(StringLiteral) || self.peek_is(Symbol)) {
                    break;
                }
                self.parse_key_value_pair(true);
                self.eat_whitespace_or_line_feeds();
            }
        }

        self.eat_whitespace_or_line_feeds();

        self.expect(RBrace);
        self.b.finish_node();
    }

    fn parse_lambda(&mut self) {
        // \x -> body
        self.b.start_node(LambdaExpr.into());
        self.expect(Backslash);

        self.parse_param_list();

        self.expect(Arrow);
        self.eat_whitespace_or_line_feeds();

        self.b.start_node(FuncBody.into());
        self.parse_expr(false);
        self.b.finish_node();
        self.b.finish_node();
    }

    fn parse_param_list(&mut self) {
        self.b.start_node(FuncParamList.into());
        while self.peek_is(Ident) {
            self.b.start_node(FuncParam.into());
            self.eat();
            self.b.finish_node();
            self.eat_whitespace_or_line_feeds();
        }
        self.b.finish_node();
    }
}
