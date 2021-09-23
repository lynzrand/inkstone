use crate::node::SynTag;

use SynTag::*;

impl SynTag {
    pub fn can_start_stmt(self) -> bool {
        self.can_start_expr() || matches!(
            self,
            // keywords
            UseKw | LetKw | DefKw
            // empty stmt
            | Semicolon
        )
    }

    pub fn can_start_expr(self) -> bool {
        matches!(
            self,
            // keywords
            BeginKw | IfKw | WhileKw | ForKw
            // expressions
            | Add | Sub | Amp | Backslash
            // literals
            | Ident | Symbol | Int | Float
            | NoninterpolatedString | InterpolatedStringStart
            | LParen | LBracket | LBrace
        )
    }

    pub fn is_opening(self) -> bool {
        matches!(self, LParen | LBracket | LBrace | IfKw | ElseKw | WhileKw | ForKw | BeginKw)
    }

    pub fn is_closing_of(self, other: SynTag) -> bool {
        matches!((self, other), 
            // parenthesis
            (LParen, RParen) | (LBracket, RBracket) | (LBrace, RBrace) 
            // conditionals
            | (IfKw, EndKw) | (IfKw, ElseKw) | (ElseKw, EndKw)
            // loops
            | (ForKw, EndKw) | (WhileKw, EndKw)
            // blocks
            | (BeginKw, EndKw)
        )
    }
}
