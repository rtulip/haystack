use super::quote::Quote;
use std::convert::From;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Function,
    If,
    Else,
    As,
    Enum,
    Impl,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    NotEqual,
    Arrow,
    Dot,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal<'src> {
    U32(u32),
    Bool(bool),
    String(&'src str),
}

impl<'src> Literal<'src> {
    fn string(&self) -> &'src str {
        match self {
            Literal::String(s) => s,
            _ => panic!("Not a string"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind<'src> {
    Keyword(Keyword),
    Symbol(Symbol),
    Literal(Literal<'src>),
    Identifier(&'src str),
    Whitespace,
    EndOfFile,
}

impl<'src> TokenKind<'src> {
    pub fn is_shape<T: Into<TokenShape>>(&self, shape: T) -> bool {
        match self {
            TokenKind::Keyword(kw) => {
                matches!(shape.into(), TokenShape::Keyword(ref keyword) if kw == keyword)
            }
            TokenKind::Symbol(sym) => {
                matches!(shape.into(), TokenShape::Symbol(ref symbol) if sym == symbol)
            }
            TokenKind::Literal(_) => matches!(shape.into(), TokenShape::Literal),
            TokenKind::Identifier(_) => matches!(shape.into(), TokenShape::Identifier),
            TokenKind::Whitespace => matches!(shape.into(), TokenShape::Whitespace),
            TokenKind::EndOfFile => matches!(shape.into(), TokenShape::EndOfFile),
        }
    }
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

#[derive(Debug, Clone, Copy)]
pub enum TokenShape {
    Keyword(Keyword),
    Symbol(Symbol),
    Literal,
    Identifier,
    Whitespace,
    EndOfFile,
}

impl From<Keyword> for TokenShape {
    fn from(value: Keyword) -> Self {
        TokenShape::Keyword(value)
    }
}

impl From<Symbol> for TokenShape {
    fn from(value: Symbol) -> Self {
        TokenShape::Symbol(value)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'src> {
    kind: TokenKind<'src>,
    quote: Quote<'src>,
}

impl<'src> Token<'src> {
    pub fn new(kind: TokenKind<'src>, quote: Quote<'src>) -> Self {
        Self { kind, quote }
    }
    pub fn kind(&self) -> &TokenKind<'src> {
        &self.kind
    }
    pub fn quote(&self) -> &Quote<'src> {
        &self.quote
    }

    pub fn literal(&self) -> &Literal<'src> {
        match &self.kind {
            TokenKind::Literal(lit) => lit,
            _ => panic!("Not a literal"),
        }
    }

    pub fn ident(&self) -> &'src str {
        match &self.kind {
            TokenKind::Identifier(ident) => ident,
            _ => panic!("Not an identifier"),
        }
    }
}
