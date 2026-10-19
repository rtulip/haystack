use std::fmt::Display;

use crate::lex::token::Token;

#[derive(Debug)]
pub enum ParseType<'src> {
    Simple(Token<'src>),
    Paramatarized {
        base: Token<'src>,
        params: Vec<Self>,
    },
    Pointer {
        inner: Box<Self>,
        mutable: bool,
    },
}

impl<'src> ParseType<'src> {
    pub fn token(&self) -> Token<'src> {
        match self {
            ParseType::Simple(token) => token.clone(),
            ParseType::Paramatarized { base, params } => base.clone(),
            ParseType::Pointer { inner, mutable } => todo!(),
        }
    }
}
