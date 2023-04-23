use crate::{
    error::HayError,
    lex::token::Token,
    types::{Type, TypeId},
};

#[derive(Debug, Clone)]
pub struct EnumStmt {
    pub token: Token,
    pub name: Token,
    pub variants: Vec<Token>,
}

impl EnumStmt {}
