use enum_ordinalize::Ordinalize;
use logos::Logos;

/// The syntax tag type.
///
/// This type is used both in lexing (as token) and in AST (as subtree tag).
#[derive(Logos, Debug, PartialEq, Eq, PartialOrd, Ord, Ordinalize)]
#[repr(u16)]
pub enum SynTag {
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
    Name,

    // Expressions
    ParenExpr,
    UnaryExpr,
    BinaryExpr,
    FunctionCallExpr,
    VarExpr,
    DotExpr,
    SubscriptExpr,
    LiteralExpr,
    IfExpr,
    WhileLoopExpr,
    ForLoopExpr,
    BlockExpr,
    LambdaExpr,

    // Statements
    ExprStmt,
    UseStmt,

    // Block-scope elements
    Block,

    FuncDef,
    FuncParamList,
    FuncParam,

    ModuleDef,

    /// The root node of the syntax tree
    Root,
}

impl SynTag {
    pub fn is_token(&self) -> bool {
        *self <= SynTag::Error
    }

    pub fn is_trivia(&self) -> bool {
        *self == SynTag::WS
    }
}

impl From<SynTag> for rowan::SyntaxKind {
    fn from(kind: SynTag) -> Self {
        rowan::SyntaxKind(kind.ordinal())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum InkstoneLang {}

impl rowan::Language for InkstoneLang {
    type Kind = SynTag;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SynTag::from_ordinal(raw.0).expect("Invalid syntax tag type!")
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.ordinal())
    }
}
