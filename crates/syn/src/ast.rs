use rowan::TextRange;

use crate::SynTag;
use crate::{SyntaxNode, SyntaxToken};

/// Generate boilerplate code for this ast node.
///
/// This macro has 2 patterns:
///
/// 1.  Expects a single [`SynTag`] and generates a struct that
///     casts the [`SyntaxNode`] with the specific tag.
/// 2.  enum
macro_rules! ast_node {
    ($name:ident, $kind:pat) => {
        #[derive(Debug, Hash, PartialEq, Eq, Clone)]
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

        impl ::std::fmt::Display for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.node)
            }
        }
    };
    ($name:ident, {
        $( $kind:pat => ($kind_name:ident, $ty:ty) ),*
    }) => {
        #[derive(Debug, Hash, PartialEq, Eq, Clone)]
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

        impl ::std::fmt::Display for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.node())
            }
        }
    };
    (token: $name:ident, $enum_name:ident, {
        $( $kind:pat => $kind_name:ident ),*
    }) => {
        #[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
        pub enum $enum_name {
            $($kind_name),*
        }

        #[derive(Debug, Hash, PartialEq, Eq, Clone)]
        pub struct $name {
            kind: $enum_name,
            node: SyntaxNode,
            token: SyntaxToken,
        }

        impl $name {
            pub fn cast(node: SyntaxNode) -> Option<Self> {
                let first_child = node.first_token()?;
                let kind = match first_child.kind() {
                    $(
                        $kind => Some($enum_name::$kind_name),
                    )*
                    _ => None
                }?;

                Some($name {
                    kind,
                    node,
                    token: first_child,
                })
            }

            pub fn node(&self) -> &SyntaxNode {
                &self.node
            }

            pub fn token(&self) -> &SyntaxToken {
                &self.token
            }

            pub fn kind(&self) -> $enum_name {
                self.kind
            }
        }

        impl ::std::fmt::Display for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{:?}: {}", self.kind(), self.node())
            }
        }
    };
}

/// Implements various methods to access a node's children.
///
/// Variants for selecting a single child (returns `None` if not available):
///
/// - `1` for the first node satisfying requirements
/// - `tok1` for the first token satisfying requirements
/// - `nth` for the `n`th node satisfying requirements
///
/// Single child methods also have a `!` variant that asserts the node always exists
/// Such variants are used if the api _requires_ the corresponding node to always exist if no errors
/// are emitted.
///
/// Variants for selecting many children (returns an iterator):
///
/// - `n` for all nodes satisfying requirements
/// - `tok` for all tokens satisfying requirements
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
    (1!, $fn_name:ident, $ty:ty) => {
        pub fn $fn_name(&self) -> $ty {
            self.node().children().find_map(<$ty>::cast)
                .expect(concat!("No such child that casts to type: ", stringify!($ty)))
        }
    };
    (1!, $fn_name:ident, $tag:pat, $ty:ty) => {
        pub fn $fn_name(&self) -> $ty {
            self.node()
                .children()
                .filter(|node| matches!(node.kind(), $tag))
                .find_map(<$ty>::cast)
                .expect(concat!("No such child using tag: ", stringify!($tag)))
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
    (tok1!, $fn_name:ident, $pat:expr) => {
        pub fn $fn_name(&self) -> SyntaxToken {
            self.node()
                .children_with_tokens()
                .filter_map(|el| el.into_token())
                .find(|tok| $pat(tok.kind()))
                .expect(concat!("No such child satisfying: ", stringify!($expr)))
        }
    };
    (nth, $fn_name:ident, $ty:ty, $n:expr) => {
        pub fn $fn_name(&self) -> Option<$ty> {
            self.node().children().filter_map(<$ty>::cast).nth($n)
        }
    };
    (nth!, $fn_name:ident, $ty:ty, $n:expr) => {
        pub fn $fn_name(&self) -> $ty {
            self.node().children().filter_map(<$ty>::cast).nth($n)
                .expect(concat!("No ", stringify!($n), "th child satisfying: ", stringify!($expr)))
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
    impl_child!(1!, block_scope, BlockScope);
}

ast_node!(BlockScope, SynTag::BlockScope);
impl BlockScope {
    impl_child!(n, stmt, Stmt);
}

ast_node!(Stmt, {
    SynTag::ExprStmt => (Expr, ExprStmt),
    SynTag::FuncDef => (Def, FuncDef),
    SynTag::LetStmt => (Let, LetStmt),
    SynTag::UseStmt => (Use, UseStmt),
    SynTag::ModuleDef => (Mod, ModuleDef)
});

ast_node!(ModuleDef, SynTag::ModuleDef);
impl ModuleDef {
    impl_child!(1, vis, Visibility);
    impl_child!(1!, name, Name);
    impl_child!(1!, block_scope, BlockScope);
}

ast_node!(ExprStmt, SynTag::ExprStmt);
impl ExprStmt {
    impl_child!(1!, expr, Expr);
}

ast_node!(Visibility, SynTag::Visibility);
impl Visibility {
    impl_child!(1, public, PublicVisibility);
}

ast_node!(PublicVisibility, SynTag::Pub);

ast_node!(LetStmt, SynTag::LetStmt);
impl LetStmt {
    impl_child!(1, vis, Visibility);
    impl_child!(1!, binding, Binding);
    impl_child!(1!, expr, Expr);
}

ast_node!(FuncDef, SynTag::FuncDef);
impl FuncDef {
    impl_child!(1, vis, Visibility);
    impl_child!(1!, name, Name);
    impl_child!(1!, param_list, FuncParamList);
    impl_child!(1!, body, Expr);
}

ast_node!(FuncParamList, SynTag::FuncParamList);
impl FuncParamList {
    impl_child!(n, params, FuncParam);
}

ast_node!(FuncParam, SynTag::FuncParam);
impl FuncParam {
    impl_child!(tok1!, name, |o| o == SynTag::Ident);
}

ast_node!(UseStmt, SynTag::UseStmt);

ast_node!(Expr, {
    SynTag::BinaryExpr       => (Binary, BinaryExpr),
    SynTag::AssignExpr       => (Assign, AssignExpr),
    SynTag::UnaryExpr        => (Unary, UnaryExpr),
    SynTag::FunctionCallExpr => (FunctionCall, FunctionCallExpr),
    SynTag::IdentExpr        => (Ident, IdentExpr),
    SynTag::ParenExpr        => (Paren, ParenExpr),
    SynTag::SubscriptExpr    => (Subscript, SubscriptExpr),
    SynTag::DotExpr          => (Dot, DotExpr),
    SynTag::IfExpr           => (If, IfExpr),
    SynTag::WhileLoopExpr    => (While, WhileLoopExpr),
    SynTag::ForLoopExpr      => (For, ForLoopExpr),
    SynTag::ReturnExpr       => (Return, ReturnExpr),
    SynTag::BreakExpr        => (Break, BreakExpr),
    SynTag::ContinueExpr     => (Continue, ContinueExpr),
    SynTag::BlockExpr        => (Block, BlockExpr),
    SynTag::LiteralExpr      => (Literal, LiteralExpr),
    SynTag::TupleLiteralExpr => (Tuple, TupleLiteralExpr),
    SynTag::ArrayLiteralExpr => (Array, ArrayLiteralExpr),
    SynTag::ObjectLiteralExpr=> (Object, ObjectLiteralExpr),
    SynTag::LambdaExpr       => (Lambda, LambdaExpr)
});

ast_node!(BinaryExpr, SynTag::BinaryExpr);
impl BinaryExpr {
    impl_child!(1!, op, BinaryOp);
    impl_child!(nth!, lhs, Expr, 0);
    impl_child!(nth!, rhs, Expr, 1);
}

ast_node!(token: BinaryOp, BinaryOpKind, {
    SynTag::OrKw => ShortCircuitOr,
    SynTag::AndKw => ShortCircuitAnd,

    SynTag::Lt => Lt,
    SynTag::Gt => Gt,
    SynTag::Le => Le,
    SynTag::Ge => Ge,
    SynTag::Eq => Eq,
    SynTag::Neq => Ne,

    SynTag::Add => Add,
    SynTag::Sub => Sub,

    SynTag::Amp => BitAnd,
    SynTag::Bar => BitOr,
    SynTag::BitXor => BitXor,

    SynTag::Mul => Mul,
    SynTag::Div => Div,
    SynTag::Rem => Rem,

    SynTag::Pow => Pow
});

ast_node!(AssignExpr, SynTag::AssignExpr);
impl AssignExpr {
    impl_child!(nth!, tgt, Expr, 0);
    impl_child!(nth!, val, Expr, 1);
}

ast_node!(UnaryExpr, SynTag::UnaryExpr);
impl UnaryExpr {
    impl_child!(1!,op,UnaryOp);
    impl_child!(1!, lhs, Expr);
}

ast_node!(token: UnaryOp, UnaryOpKind, {
    SynTag::NotKw   => NotKw,
    SynTag::Add     => Pos,
    SynTag::Sub     => Neg,
    SynTag::Not     => Not
});

ast_node!(FunctionCallExpr, SynTag::FunctionCallExpr);
impl FunctionCallExpr {
    impl_child!(1!, func, Expr);
    impl_child!(n, params, Expr, skip = 1);
}

ast_node!(IdentExpr, SynTag::IdentExpr);
impl IdentExpr {
    impl_child!(tok1!, ident, |o| o == SynTag::Ident);
}

ast_node!(SubscriptExpr, SynTag::SubscriptExpr);
impl SubscriptExpr {
    impl_child!(nth!, parent, Expr, 0);
    impl_child!(nth!, subscript, Expr, 1);
}

ast_node!(DotExpr, SynTag::DotExpr);
impl DotExpr {
    impl_child!(nth!, parent, Expr, 0);
    impl_child!(tok1!, subscript, |o| o == SynTag::Ident);
}

ast_node!(LambdaExpr, SynTag::LambdaExpr);
impl LambdaExpr {
    impl_child!(1!, param_list, FuncParamList);
    impl_child!(1!, body, Expr);
}

ast_node!(Chunk, SynTag::BlockScope);

ast_node!(IfExpr, SynTag::IfExpr);
impl IfExpr {
    impl_child!(n, branches, IfBranch);
}

ast_node!(IfBranch, SynTag::IfBranch);
impl IfBranch {
    impl_child!(1, condition, Condition);
    impl_child!(1!, body, Expr);
}

ast_node!(Condition, SynTag::Condition);
impl Condition {
    impl_child!(1!, expr, Expr);
}

ast_node!(WhileLoopExpr, SynTag::WhileLoopExpr);
impl WhileLoopExpr {
    impl_child!(1!, condition, Condition);
    impl_child!(1!, body, BlockScope);
}

ast_node!(ForLoopExpr, SynTag::ForLoopExpr);
impl ForLoopExpr {
    impl_child!(1!, condition, ForCondition);
    impl_child!(1!, body, BlockScope);
}

ast_node!(ForCondition, SynTag::Condition);
impl ForCondition {
    impl_child!(1!, binding, Binding);
    impl_child!(1!, expr, Expr);
}

ast_node!(ReturnExpr, SynTag::ReturnExpr);
impl ReturnExpr {
    impl_child!(1, expr, Expr);
}

ast_node!(BreakExpr, SynTag::BreakExpr);
impl BreakExpr {
    impl_child!(1, expr, Expr);
}

ast_node!(ContinueExpr, SynTag::ContinueExpr);

ast_node!(BlockExpr, SynTag::BlockExpr);
impl BlockExpr {
    impl_child!(1!, scope, BlockScope);
}

ast_node!(token: LiteralExpr, LiteralKind, {
    SynTag::Int => Int,
    SynTag::Float => Float,
    SynTag::StringLiteral => String,
    SynTag::Symbol => Symbol,
    SynTag::TrueKw => True,
    SynTag::FalseKw => False,
    SynTag::NilKw => Nil
});

ast_node!(ParenExpr, SynTag::ParenExpr);
impl ParenExpr {
    impl_child!(1!, inner, Expr);
}

ast_node!(TupleLiteralExpr, SynTag::TupleLiteralExpr);
impl TupleLiteralExpr {
    impl_child!(n, items, Expr);
}

ast_node!(ArrayLiteralExpr, SynTag::ArrayLiteralExpr);
impl ArrayLiteralExpr {
    impl_child!(n, items, Expr);
}

ast_node!(ObjectLiteralExpr, SynTag::ObjectLiteralExpr);

ast_node!(Binding, SynTag::Binding);
impl Binding {
    impl_child!(tok1, name, |o| o == SynTag::Ident);
}

ast_node!(Name, SynTag::Name);
impl Name {
    impl_child!(tok1!, name, |o| o == SynTag::Ident);
}
