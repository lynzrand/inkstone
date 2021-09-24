use rowan::{GreenNode, GreenNodeBuilder, SyntaxNode};

mod tag_util;
#[cfg(test)]
mod test;

use crate::node::InkstoneLang;
use crate::node::SynTag::{self, *};
use crate::Lexer;

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

    fn eat(&mut self) -> Option<SynTag> {
        self.eat_if(|_| true)
    }

    fn peek(&mut self) -> Option<SynTag> {
        self.lexer.peek()
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
}

/// The concrete parsing implementations.
///
/// # Note
///
/// - Parsing methods should take care of any whitespace or line feed **after**
///   them, but not **before** them.
/// - Whitespace tokens around a node should be placed **outside** that node.
impl<'src> Parser<'src> {
    fn parse_root(&mut self) {
        self.b.start_node(Root.into());
        self.eat_whitespace_or_line_feeds();

        self.parse_block_scope();

        self.b.finish_node();
    }

    fn parse_block_scope(&mut self) {
        self.b.start_node(BlockScope.into());

        while self.peek_if(|t| t.can_start_stmt()) {
            self.parse_stmt();
        }
        self.b.finish_node();

        self.eat_whitespace_or_line_feeds();
    }

    fn parse_block(&mut self) {
        self.b.start_node(Block.into());
        assert!(
            self.eat() == Some(BeginKw),
            "Block parsing must begin with `begin`"
        );
        self.eat_whitespace_or_line_feeds();

        self.parse_block_scope();

        self.eat();

        self.b.finish_node();
        self.eat_whitespace_or_line_feeds();
    }

    fn parse_stmt(&mut self) {
        if self.peek_if(|t| t.can_start_expr()) {
            self.parse_expr();
        }
        self.eat_whitespace_or_line_feeds();
    }

    fn parse_expr(&mut self) {
        debug_assert!(
            self.peek().unwrap().can_start_expr(),
            "Must start with some token that can start an expression"
        );

        match self.peek().unwrap() {
            BeginKw => {
                self.parse_block();
            }
            _ => {
                todo!();
            }
        }

        self.eat_whitespace();
    }
}
