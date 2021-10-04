use std::{collections::VecDeque, ops::Range};

use logos::Span;

pub mod ast;
pub mod node;
pub mod parse;

pub use node::InkstoneLang;
pub use node::SynTag;

pub type SyntaxNode = rowan::SyntaxNode<InkstoneLang>;
pub type SyntaxToken = rowan::SyntaxToken<InkstoneLang>;
pub type SyntaxElement = rowan::SyntaxElement<InkstoneLang>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<InkstoneLang>;

/// The main lexer used in Inkstone.
pub struct Lexer<'lex> {
    /// The actual lexer that does the job.
    inner: logos::Lexer<'lex, SynTag>,
    /// The span of the last token
    span: Span,
    /// Tokens that has been splitted and put back into lexer.
    pending_tokens: VecDeque<(SynTag, Span)>,
    /// Whether to ignore newlines when lexing.
    ignore_newline: bool,
}

impl<'lex> Lexer<'lex> {
    /// Create a new lexer from string.
    pub fn new(s: &'lex str) -> Lexer<'lex> {
        Lexer {
            inner: logos::Lexer::new(s),
            span: Default::default(),
            pending_tokens: VecDeque::new(),
            ignore_newline: false,
        }
    }

    /// The underlying source code
    pub fn source(&self) -> &'lex str {
        self.inner.source()
    }

    /// Whether to ignore newline tokens at current position.
    ///
    /// This option is used for optimal lexing of multiline language parts. Here's an example:
    ///
    /// ```plaintext
    /// let foo = (my_func 1 2 3) + 4 * 5
    /// #  ^A B^ ^ ^D       ^F   ^H  ^J  ^L
    /// #        C        ^E  ^G   ^I  ^K
    /// ```
    ///
    /// In this example, we can ignore newlines without ambiguity after operators (position `I`, `K`),
    /// inside parentheses (position `D`--`G`) or in language structures (position `A`--`C`),
    /// but not before operators (position `H` and `J`).
    pub fn ignore_newline(&mut self, value: bool) {
        self.ignore_newline = value
    }

    /// The span of the current token. Directly exported from the inner tokenizer.
    pub fn span(&self) -> Range<usize> {
        self.span.clone()
    }

    pub fn peek_span(&mut self) -> Range<usize> {
        if self.pending_tokens.is_empty() {
            self.peek();
        }
        self.pending_tokens
            .front()
            .map(|(_, s)| s.clone())
            .unwrap_or_default()
    }

    pub fn peek_slice(&mut self) -> &'lex str {
        &self.inner.source()[self.peek_span()]
    }

    /// The slice of string of the current token. Directly exported from the inner tokenizer.
    pub fn slice(&self) -> &'lex str {
        &self.inner.source()[self.span()]
    }

    /// Return a copy of the current front token without really consuming it.
    pub fn peek(&mut self) -> Option<SynTag> {
        if self.pending_tokens.is_empty() {
            let next = self.inner.next()?;
            self.pending_tokens.push_front((next, self.inner.span()));
        }

        debug_assert!(!self.pending_tokens.is_empty());

        self.pending_tokens.front().map(|(tag, _)| *tag)
    }
}

impl<'lex> Iterator for Lexer<'lex> {
    type Item = SynTag;

    fn next(&mut self) -> Option<Self::Item> {
        // check pending tokens or lex the next one
        if let Some((tok, span)) = self.pending_tokens.pop_front() {
            self.span = span;
            Some(tok)
        } else {
            let tok = self.inner.next();
            self.span = self.inner.span();
            tok
        }
    }
}
