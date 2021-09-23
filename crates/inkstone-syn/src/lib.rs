use std::{collections::VecDeque, ops::Range};

use node::SynTag;

pub mod ast;
pub mod node;
pub mod parse;

/// The main lexer used in Inkstone.
pub struct Lexer<'lex> {
    /// The actual lexer that does the job.
    inner: logos::Lexer<'lex, SynTag>,
    /// Tokens that has been splitted and put back into lexer.
    pending_tokens: VecDeque<SynTag>,
    /// Whether to ignore newlines when lexing.
    ignore_newline: bool,
}

impl<'lex> Lexer<'lex> {
    /// Create a new lexer from string.
    pub fn new(s: &'lex str) -> Lexer<'lex> {
        Lexer {
            inner: logos::Lexer::new(s),
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
        self.inner.span()
    }

    /// The slice of string of the current token. Directly exported from the inner tokenizer.
    pub fn slice(&self) -> &'lex str {
        self.inner.slice()
    }

    /// Return a copy of the current front token without really consuming it.
    pub fn peek(&mut self) -> Option<SynTag> {
        if self.pending_tokens.is_empty() {
            let next = self.next()?;
            self.pending_tokens.push_front(next);
        }

        debug_assert!(!self.pending_tokens.is_empty());

        self.pending_tokens.front().copied()
    }

    /// Push tokens back into the lexer. This method pushes to the _front_ of the token list, and
    /// follows a LIFO rule. Tokens will be pushed before those [`backtrack_back`] pushed.
    pub fn backtrack_front(&mut self, tok: SynTag) {
        self.pending_tokens.push_front(tok)
    }

    /// Push tokens back into the lexer. This method pushes to the _back_ of the token list, and
    /// follows a FIFO rule. Tokens will be pushed after those [`backtrack_front`] pushed.
    pub fn backtrack_back(&mut self, tok: SynTag) {
        self.pending_tokens.push_back(tok)
    }
}

impl<'lex> Iterator for Lexer<'lex> {
    type Item = SynTag;

    fn next(&mut self) -> Option<Self::Item> {
        // check pending tokens or lex the next one
        self.pending_tokens
            .pop_front()
            .or_else(|| self.inner.next())
    }
}
