use rowan::TextRange;

use crate::SynTag;
use crate::{SyntaxNode, SyntaxNodeChildren};

/// Generate boilerplate code for this ast node.
///
/// This macro has 2 patterns:
///
/// 1.  Expects a single [`SynTag`] and generates a struct that
///     casts the [`SyntaxNode`] with the specific tag.
/// 2.  enum
macro_rules! ast_node {
    ($name:ident, $kind:pat) => {
        #[derive(Debug, Hash, PartialEq, Eq)]
        pub struct $name {
            node: SyntaxNode,
        }

        impl AstNode for $name {
            fn cast(node: SyntaxNode) -> Option<Self> {
                match node.kind() {
                    $kind => Some(Self{ node }),
                    _ => None,
                }
            }

            fn node(&self) -> &SyntaxNode {
                &self.node
            }
        }
    };
    ($name:ident, {
        $( $kind:pat => ($kind_name:ident, $ty:ty) ),*
    }) => {
        #[derive(Debug, Hash, PartialEq, Eq)]
        pub enum $name {
            $($kind_name($ty)),*
        }

        impl AstNode for $name {
            fn cast(node: SyntaxNode) -> Option<Self> {
                match node.kind() {
                    $(
                        $kind => Some(Self::$kind_name(
                            <$ty>::cast(node)
                                .expect(concat!(
                                    "Unable to cast `",
                                    stringify!($kind),
                                    "` to type `",
                                    stringify!($ty),
                                    "`"
                                ))
                            )),
                    )*
                    _=> None
                }
            }

            fn node(&self) -> &SyntaxNode {
                match self { $(
                    Self::$kind_name(val) => val.node(),
                )* }
            }
        }
    }
}

pub trait AstNode: Sized {
    /// Cast the specified syntax node into this type
    fn cast(node: SyntaxNode) -> Option<Self>;

    /// Get the corresponding syntax node of this type
    fn node(&self) -> &SyntaxNode;

    /// Get the text span of this type
    fn span(&self) -> TextRange {
        self.node().text_range()
    }
}

ast_node!(Root, SynTag::Root);

ast_node!(BlockScope, SynTag::BlockScope);

ast_node!(Stmt, {
    SynTag::ExprStmt => (Expr, ExprStmt),
    SynTag::FuncDef => (Def, FuncDef),
    SynTag::LetStmt => (Let, LetStmt),
    SynTag::UseStmt => (Use, UseStmt)
});

ast_node!(ExprStmt, SynTag::ExprStmt);
ast_node!(LetStmt, SynTag::LetStmt);
ast_node!(FuncDef, SynTag::FuncDef);
ast_node!(UseStmt, SynTag::UseStmt);

ast_node!(Expr, {
    SynTag::BinaryExpr       => (Binary, BinaryExpr),
    SynTag::UnaryExpr        => (Unary, UnaryExpr),
    SynTag::FunctionCallExpr => (FunctionCall, FunctionCallExpr),
    SynTag::VarExpr          => (Var, BinaryExpr),
    SynTag::Name             => (Name, NameExpr),
    SynTag::SubscriptExpr    => (Subscript, SubscriptExpr),
    SynTag::DotExpr          => (Dot, DotExpr),
    SynTag::IfExpr           => (If, IfExpr),
    SynTag::WhileLoopExpr    => (While, WhileLoopExpr),
    SynTag::ForLoopExpr      => (For, ForLoopExpr),
    SynTag::BlockExpr        => (Block, BlockExpr),
    SynTag::LiteralExpr      => (Literal, LiteralExpr)
});

ast_node!(BinaryExpr, SynTag::BinaryExpr);
ast_node!(UnaryExpr, SynTag::UnaryExpr);
ast_node!(FunctionCallExpr, SynTag::FunctionCallExpr);
ast_node!(VarExpr, SynTag::VarExpr);
ast_node!(NameExpr, SynTag::Name);
ast_node!(SubscriptExpr, SynTag::SubscriptExpr);
ast_node!(DotExpr, SynTag::DotExpr);
ast_node!(IfExpr, SynTag::IfExpr);
ast_node!(WhileLoopExpr, SynTag::WhileLoopExpr);
ast_node!(ForLoopExpr, SynTag::ForLoopExpr);
ast_node!(BlockExpr, SynTag::BlockExpr);
ast_node!(LiteralExpr, SynTag::LiteralExpr);
