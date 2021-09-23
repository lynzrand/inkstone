use std::convert::TryFrom;

use logos::Logos;
use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Logos, Debug, PartialEq, Eq, PartialOrd, Ord, IntoPrimitive, TryFromPrimitive)]
#[repr(u16)]
pub enum Syntax {
    // =========== Tokens ============
    // === Aux Tokens ===
    /// Whitespace
    #[regex(r"\s")]
    WS,
    /// End of line
    #[regex(r"\n|\r|\r\n")]
    Eol,

    // === Keywords ===
    #[token("begin")]
    BeginKw,
    #[token("end")]
    EndKw,
    #[token("use")]
    UseKw,
    #[token("let")]
    LetKw,
    #[token("def")]
    DefKw,
    // #[token("implicit")]
    // ImplicitKw,
    #[token("if")]
    IfKw,
    #[token("else")]
    ElseKw,
    #[token("while")]
    WhileKw,
    #[token("for")]
    ForKw,
    #[token("in")]
    InKw,
    #[token("mod")]
    ModKw,
    // `and`, `or` and `not` are both keywords and operators
    #[token("and")]
    AndKw,
    #[token("or")]
    OrKw,
    #[token("not")]
    NotKw,

    // === Identifiers ====
    #[regex(r"[a-zA-Z_][0-9a-zA-Z_?!]*")]
    Ident,

    // === Literal Tokens ===
    #[regex(r":[a-zA-Z_][0-9a-zA-Z_?!]*")]
    Symbol,
    #[regex(r"-?0|0x[0-9a-fA-F_]+|[0-9]+")]
    Int,
    #[regex(r"[+-]?[0-9]*\.[0-9]+([eE][+-]?[0-9]+)")]
    Float,

    /// An RFC8259-compliant string. Invalid cases are handled afterward.
    #[regex(r#""(([^\r\n\\$"]|\\.)*)""#)]
    NoninterpolatedString,
    #[regex(r#""([^\r\n\\$"]|\\.)*\$"#)]
    InterpolatedStringStart,
    #[regex(r#"([^\r\n\\$"]|\\.)*\$"#)]
    InterpolatedStringMiddle,
    #[regex(r#"([^\r\n\\$"]|\\.)*""#)]
    InterpolatedStringEnd,

    // === Operators ===
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("**")]
    Pow,
    #[token("%")]
    Rem,
    #[token(">")]
    Gt,
    #[token(">=")]
    Ge,
    #[token("<")]
    Lt,
    #[token("<=")]
    Le,
    #[token("==")]
    Eq,
    #[token("!=")]
    Neq,
    #[token("=")]
    Assign,
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("&")]
    Amp,
    #[token("|")]
    Bar,
    #[token("^")]
    BixXor,
    #[token("!")]
    Not,
    #[token(":")]
    Colon,
    #[token("\\")]
    Backslash,
    #[token("->")]
    Arrow,
    #[token(";")]
    Semicolon,
    #[token("::")]
    DoubleColon,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,

    /// Anything that doesn't match. Also the last token entry.
    #[error]
    Error,

    // ================ AST Nodes ================
    // Expressions
    Expr,

    // Statements
    Stmt,

    // Block-scope elements
    Block,
    FuncDef,
    Lambda,
    ModuleDef,
}

impl Syntax {
    pub fn is_token(&self) -> bool {
        *self <= Syntax::Error
    }
}

impl From<Syntax> for rowan::SyntaxKind {
    fn from(kind: Syntax) -> Self {
        rowan::SyntaxKind(kind.into())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum InkstoneLang {}

impl rowan::Language for InkstoneLang {
    type Kind = Syntax;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        Syntax::try_from(raw.0).expect("Invalid syntax tag type!")
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.into())
    }
}