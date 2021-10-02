use std::fmt::{Display, Formatter};

use crate::node::{InkstoneLang, SynTag};

use rowan::SyntaxNode;
use SynTag::*;

impl SynTag {
    pub fn can_start_stmt(self) -> bool {
        self.can_start_expr()
            || matches!(
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
            BeginKw | IfKw | WhileKw | ForKw | BreakKw | ReturnKw
            // expressions
            | Add | Sub | Amp | Backslash
            // literals
            | Ident | Symbol | Int | Float
            | StringLiteral | TrueKw | FalseKw
            // | InterpolatedStringStart
            | LParen | LBracket | LBrace
        )
    }

    pub fn is_opening(self) -> bool {
        matches!(
            self,
            LParen | LBracket | LBrace | IfKw | ElseKw | WhileKw | ForKw | BeginKw
        )
    }

    pub fn is_closing_of(self, other: SynTag) -> bool {
        matches!(
            (self, other),
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

    pub fn is_expr_parsing_sync_token(self) -> bool {
        self.is_stmt_parsing_sync_token() || matches!(self, Comma | RParen | RBrace | RBracket)
    }

    pub fn is_stmt_parsing_sync_token(self) -> bool {
        matches!(self, EndKw | LF | Semicolon)
    }

    pub fn is_block_parsing_sync_token(self) -> bool {
        matches!(self, EndKw)
    }
}

pub struct FormatTree<'a>(pub &'a SyntaxNode<InkstoneLang>);

impl<'a> Display for FormatTree<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        format_tree(self.0, f)
    }
}

fn format_tree(node: &SyntaxNode<InkstoneLang>, f: &mut Formatter) -> std::fmt::Result {
    let mut depth = 0;
    for ev in node.preorder_with_tokens() {
        match ev {
            rowan::WalkEvent::Enter(e) => {
                for _ in 0..depth {
                    f.write_str("    ")?;
                }
                match e {
                    rowan::NodeOrToken::Node(n) => writeln!(f, "{:?}", n.kind()),
                    rowan::NodeOrToken::Token(t) => writeln!(f, "{:?} {:?}", t.kind(), t.text()),
                }?;
                depth += 1;
            }
            rowan::WalkEvent::Leave(_) => {
                depth -= 1;
            }
        }
    }
    Ok(())
}
