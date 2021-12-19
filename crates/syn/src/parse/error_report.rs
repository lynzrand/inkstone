use std::convert::TryInto;

use rowan::TextRange;

use crate::node::SynTag;

#[derive(Debug, Clone, Copy)]
pub struct ParseErrorSignal;
pub type Result<T> = std::result::Result<T, ParseErrorSignal>;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub span: rowan::TextRange,
    pub kind: ParseErrorKind,
    pub level: ParseErrorLevel,
}

impl ParseError {
    pub fn new(span: rowan::TextRange, kind: ParseErrorKind, level: ParseErrorLevel) -> Self {
        Self { span, kind, level }
    }

    pub fn error(span: rowan::TextRange, kind: ParseErrorKind) -> Self {
        Self {
            span,
            kind,
            level: ParseErrorLevel::Error,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    Unexpected(SynTag),
    ExpectStmt,
    ExpectExpr,
    Expected {
        expected: SynTag,
        got: Option<SynTag>,
    },
    ExpectedString {
        expected: String,
        got: Option<SynTag>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, PartialOrd, Ord)]
pub enum ParseErrorLevel {
    Info,
    Warning,
    Error,
}

pub trait IntoTextRange {
    fn into_text_range(self) -> TextRange;
}

impl IntoTextRange for std::ops::Range<usize> {
    fn into_text_range(self) -> TextRange {
        TextRange::new(
            self.start.try_into().expect("Input too large"),
            self.end.try_into().expect("Input too large"),
        )
    }
}

pub trait ParseResult {
    fn ok_parse_result() -> Self;
}

impl ParseResult for () {
    fn ok_parse_result() -> Self {}
}

impl ParseResult for Result<()> {
    fn ok_parse_result() -> Self {
        Ok(())
    }
}

impl ParseResult for bool {
    fn ok_parse_result() -> Self {
        true
    }
}
