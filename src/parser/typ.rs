use std::fmt::Display;

use crate::{lex::token::Token, parser::signature::Signature};
#[derive(Debug, Clone)]
pub struct ParamatarizedIdent<'src> {
    pub base: Token<'src>,
    pub params: Vec<ParseType<'src>>,
}
#[derive(Debug, Clone)]
pub struct ParseTypePath<'src> {
    pub path: Vec<ParamatarizedIdent<'src>>,
}

#[derive(Debug, Clone)]
pub enum ParseType<'src> {
    Simple(ParamatarizedIdent<'src>),
    Pointer { inner: Box<Self>, mutable: bool },
    Path(ParseTypePath<'src>),
    Tuple(Vec<Self>),
    Never(Token<'src>),
    Function(Token<'src>, Signature<'src>),
}

impl<'src> ParseType<'src> {
    pub fn token(&self) -> Token<'src> {
        match self {
            ParseType::Simple(ident) => ident.base.clone(),
            ParseType::Path(p) => p.path[0].base.clone(),
            ParseType::Pointer { inner, mutable } => todo!(),
            ParseType::Tuple(_) => todo!(),
            ParseType::Never(token) => token.clone(),
            ParseType::Function(token, _) => token.clone(),
        }
    }
}
