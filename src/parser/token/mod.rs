use super::quote::Quote;
use std::convert::From;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Function,
    If,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Symbol {
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Plus,
    Minus,
    GreaterThan,
    GreaterEqual,
    LessThan,
    LessEqual,
    Equals,
}

impl Symbol {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal<'src> {
    U32(u32),
    Bool(bool),
    String(&'src str),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind<'src> {
    Keyword(Keyword),
    Symbol(Symbol),
    Literal(Literal<'src>),
    Identifier(&'src str),
    Whitespace,
}

impl<'src> From<Keyword> for TokenKind<'src> {
    fn from(value: Keyword) -> Self {
        Self::Keyword(value)
    }
}

impl<'src> From<Symbol> for TokenKind<'src> {
    fn from(value: Symbol) -> Self {
        Self::Symbol(value)
    }
}

impl<'src> From<Literal<'src>> for TokenKind<'src> {
    fn from(value: Literal<'src>) -> Self {
        Self::Literal(value)
    }
}

impl<'src> From<&'src str> for TokenKind<'src> {
    fn from(value: &'src str) -> Self {
        Self::Identifier(value)
    }
}

pub struct Token<'src> {
    kind: TokenKind<'src>,
    quote: Quote<'src>,
}

impl<'src> Token<'src> {
    pub fn new(kind: TokenKind<'src>, quote: Quote<'src>) -> Self {
        Self { kind, quote }
    }
    pub fn kind(self) -> TokenKind<'src> {
        self.kind
    }
    pub fn quote(self) -> Quote<'src> {
        self.quote
    }
}
