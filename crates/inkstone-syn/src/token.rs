use logos::{Lexer, Logos};

#[derive(Logos)]
pub enum Token<'lex> {
    // === Aux Tokens ===
    /// Whitespace
    #[regex(r"\s")]
    WS,
    /// End of line
    #[regex(r"\n|\r|\r\n")]
    Eol,

    /// Anything that doesn't match
    #[error]
    Error,

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
    #[regex(r#""(([^\r\n\\$"]|\\.)*)""#, strip_string_start_end)]
    NoninterpolatedString(&'lex str),
    #[regex(r#""([^\r\n\\$"]|\\.)*\$"#, strip_string_start_end)]
    InterpolatedStringStart(&'lex str),
    #[regex(r#"([^\r\n\\$"]|\\.)*\$"#, strip_string_end)]
    InterpolatedStringMiddle(&'lex str),
    #[regex(r#"([^\r\n\\$"]|\\.)*""#, strip_string_end)]
    InterpolatedStringEnd(&'lex str),

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
}

fn strip_string_start_end<'lex>(lex: &Lexer<'lex, Token<'lex>>) -> &'lex str {
    let slice = lex.slice();
    &slice[1..slice.len() - 1]
}

fn strip_string_end<'lex>(lex: &Lexer<'lex, Token<'lex>>) -> &'lex str {
    let slice = lex.slice();
    &slice[0..slice.len() - 1]
}
