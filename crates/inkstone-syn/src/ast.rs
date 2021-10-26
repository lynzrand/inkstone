use rowan::TextRange;

use crate::SynTag;
use crate::{SyntaxNode, SyntaxNodeChildren, SyntaxToken};

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

/// Implements various methods to access a node's children.
macro_rules! impl_child {
    (1, $fn_name:ident, $ty:ty) => {
        pub fn $fn_name(&self) -> Option<$ty> {
            self.node().children().find_map(<$ty>::cast)
        }
    };
    (1, $fn_name:ident, $tag:pat, $ty:ty) => {
        pub fn $fn_name(&self) -> Option<$ty> {
            self.node()
                .children()
                .filter(|node| matches!(node.kind(), $tag))
                .find_map(<$ty>::cast)
        }
    };
    (tok1, $fn_name:ident, $pat:expr) => {
        pub fn $fn_name(&self) -> Option<SyntaxToken> {
            self.node()
                .children_with_tokens()
                .filter_map(|el| el.into_token())
                .find(|tok| $pat(tok.kind()))
        }
    };
    (nth, $fn_name:ident, $ty:ty, $n:expr) => {
        pub fn $fn_name(&self) -> Option<$ty> {
            self.node().children().filter_map(<$ty>::cast).nth($n)
        }
    };
    (n, $fn_name:ident, $ty:ty $(, skip = $skip:expr)?) => {
        pub fn $fn_name(&self) -> impl Iterator<Item = $ty> + '_ {
            self.node().children().filter_map(<$ty>::cast)$(.skip($skip))?
        }
    };
    (n, $fn_name:ident, $tag:pat, $ty:ty $(, skip = $skip:expr)?) => {
        pub fn $fn_name(&self) -> impl Iterator<Item = $ty> + '_ {
            self.node()
                .children()
                .filter(|node| matches!(node.kind(), $tag))
                .filter_map(<$ty>::cast)
                $(.skip($skip))?
        }
    };
    (tok, $fn_name:ident, $pat:expr $(, skip = $skip:expr)?) => {
        pub fn $fn_name(&self) -> impl Iterator<Item = SyntaxToken> + '_ {
            self.node()
                .children_with_tokens()
                .filter_map(|el| el.into_token())
                .filter(|tok| $pat(tok.kind()))
                $(.skip($skip))?
        }
    };
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
impl Root {
    impl_child!(1, block_scope, BlockScope);
}

ast_node!(BlockScope, SynTag::BlockScope);
impl BlockScope {
    impl_child!(n, stmt, Stmt);
}

ast_node!(Stmt, {
    SynTag::ExprStmt => (Expr, ExprStmt),
    SynTag::FuncDef => (Def, FuncDef),
    SynTag::LetStmt => (Let, LetStmt),
    SynTag::UseStmt => (Use, UseStmt)
});

ast_node!(ExprStmt, SynTag::ExprStmt);
impl ExprStmt {
    impl_child!(1, expr, Expr);
}

ast_node!(LetStmt, SynTag::LetStmt);
impl LetStmt {
    impl_child!(1, binding, Binding);
    impl_child!(1, expr, Expr);
}

ast_node!(FuncDef, SynTag::FuncDef);
ast_node!(UseStmt, SynTag::UseStmt);

ast_node!(Expr, {
    SynTag::BinaryExpr       => (Binary, BinaryExpr),
    SynTag::UnaryExpr        => (Unary, UnaryExpr),
    SynTag::FunctionCallExpr => (FunctionCall, FunctionCallExpr),
    SynTag::IdentExpr          => (Var, BinaryExpr),
    SynTag::Namespace        => (Namespace, NamespaceExpr),
    SynTag::SubscriptExpr    => (Subscript, SubscriptExpr),
    SynTag::DotExpr          => (Dot, DotExpr),
    SynTag::IfExpr           => (If, IfExpr),
    SynTag::WhileLoopExpr    => (While, WhileLoopExpr),
    SynTag::ForLoopExpr      => (For, ForLoopExpr),
    SynTag::BlockExpr        => (Block, BlockExpr),
    SynTag::LiteralExpr      => (Literal, LiteralExpr)
});

ast_node!(BinaryExpr, SynTag::BinaryExpr);
impl BinaryExpr {
    impl_child!(tok1, op, |o: SynTag| {
        crate::parse::pratt_util::infix_binding_power(o).is_some()
    });
    impl_child!(nth, lhs, Expr, 1);
    impl_child!(nth, rhs, Expr, 2);
}

ast_node!(UnaryExpr, SynTag::UnaryExpr);
impl UnaryExpr {
    impl_child!(tok1, op, |o: SynTag| {
        crate::parse::pratt_util::infix_binding_power(o).is_some()
    });
    impl_child!(nth, lhs, Expr, 1);
}

ast_node!(FunctionCallExpr, SynTag::FunctionCallExpr);
impl FunctionCallExpr {
    impl_child!(1, func, Expr);
    impl_child!(n, params, Expr, skip = 1);
}

ast_node!(VarExpr, SynTag::IdentExpr);
impl VarExpr {
    impl_child!(tok1, ident, |o| o == SynTag::Ident);
}

ast_node!(NamespaceExpr, SynTag::Namespace);
impl NamespaceExpr {
    impl_child!(tok, names, |o| o == SynTag::Ident);
}

ast_node!(SubscriptExpr, SynTag::SubscriptExpr);
impl SubscriptExpr {
    impl_child!(nth, parent, Expr, 1);
    impl_child!(nth, subscript, Expr, 2);
}

ast_node!(DotExpr, SynTag::DotExpr);
impl DotExpr {
    impl_child!(nth, parent, Expr, 1);
    impl_child!(tok1, subscript, |o| o == SynTag::Ident);
}

ast_node!(IfExpr, SynTag::IfExpr);
ast_node!(WhileLoopExpr, SynTag::WhileLoopExpr);
ast_node!(ForLoopExpr, SynTag::ForLoopExpr);
ast_node!(BlockExpr, SynTag::BlockExpr);
ast_node!(LiteralExpr, SynTag::LiteralExpr);

ast_node!(Binding, SynTag::Binding);

ast_node!(Name, SynTag::Name);
