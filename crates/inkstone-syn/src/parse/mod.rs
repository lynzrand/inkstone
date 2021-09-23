use rowan::GreenNodeBuilder;

use crate::node::SynTag::{self, *};
use crate::Lexer;

pub struct Parser<'src> {
    l: Lexer<'src>,
    b: GreenNodeBuilder<'src>,
    e: Vec<String>,
}

impl<'src> Parser<'src> {
    pub fn new(s: &'src str) -> Self {
        Parser {
            l: Lexer::new(s),
            b: GreenNodeBuilder::new(),
            e: Vec::new(),
        }
    }

    pub fn parse(&mut self) {
        self.parse_root()
    }

    fn log_err(&mut self, err: String) {
        self.e.push(err)
    }

    fn try_eat<F: FnOnce(SynTag) -> bool>(&mut self, f: F) -> bool {
        let tok = self.l.next();
        if tok.is_none() {
            return false;
        }
        let tok = tok.unwrap();
        debug_assert!(tok.is_token());

        if f(tok) {
            self.b.token(tok.into(), self.l.slice());
            true
        } else {
            self.l.backtrack_front(tok);
            false
        }
    }

    fn eat(&mut self) -> bool {
        self.try_eat(|_| true)
    }

    fn expect(&mut self, tok: SynTag) {
        debug_assert!(tok.is_token());

        if !self.try_eat(|t| t == tok) {
            // todo
        }
    }
}

impl<'src> Parser<'src> {
    fn parse_root(&mut self) {
        self.b.start_node(Root.into());
        self.parse_block_scope();
        self.b.finish_node();
    }

    fn parse_block_scope(&mut self) {
        self.b.start_node(BlockScope.into());

        self.b.finish_node();
    }

    fn parse_block(&mut self) {
        self.b.start_node(Block.into());

        self.b.finish_node();
    }

    fn parse_stmt(&mut self) {}
}
