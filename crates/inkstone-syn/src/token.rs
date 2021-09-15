use logos;
use logos::Logos;

#[derive(Logos)]
pub enum Token<'lex> {
    // === Aux Tokens ===
    /// Whitespace
    #[regex(r"\s")]
    WS,
    /// End of line
    #[regex(r"\n|\r|\r\n")]
    EOL,

    /// Anything that doesn't match
    #[error]
    Error,

    // === Keywords ===
    #[token("do")]
    DoKw,
    #[token("end")]
    EndKw,
    #[token("use")]
    UseKw,
    #[token("let")]
    LetKw,
    #[token("def")]
    DefKw,
    #[token("implicit")]
    ImplicitKw,
    #[token("if")]
    IfKw,
    #[token("else")]
    ElseKw,
    #[token("while")]
    WhileKw,
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
    #[regex(r#""(([^\r\n\\"]|\\.)*)""#)]
    NoninterpolatedString(&'lex str),
    #[regex(r#""([^\r\n\\"]|\\.)*\$"#)]
    InterpolatedStringStart,
    #[regex(r#"([^\r\n\\"]|\\.)*\$"#)]
    InterpolatedStringMiddle,
    #[regex(r#"([^\r\n\\"]|\\.)*""#)]
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
}
