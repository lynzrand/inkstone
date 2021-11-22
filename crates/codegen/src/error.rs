use std::borrow::Cow;

use rowan::TextRange;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum ErrorLevel {
    Allow,
    Note,
    Warning,
    Error,
}

#[derive(Debug)]
pub struct CompileError {
    level: ErrorLevel,
    id: &'static str,
    span: TextRange,
    message: Option<Cow<'static, str>>,
}

impl CompileError {
    pub fn new(id: &'static str, span: TextRange) -> CompileError {
        CompileError {
            level: ErrorLevel::Error,
            id,
            span,
            message: None,
        }
    }

    pub fn with_message(mut self, message: impl Into<Cow<'static, str>>) -> Self {
        self.message = Some(message.into());
        self
    }

    pub fn with_level(mut self, level: ErrorLevel) -> Self {
        self.level = level;
        self
    }
}
