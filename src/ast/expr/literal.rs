use crate::{
    error::HayError,
    lex::token::{Literal, Token},
    types::{Type, TypeId},
};

#[derive(Debug, Clone)]
pub struct ExprLiteral {
    pub literal: Literal,
    pub token: Token,
}

impl ExprLiteral {}
