use crate::{error::HayError, lex::token::Token};

#[derive(Debug, Clone)]
pub struct ExprReturn {
    pub token: Token,
}

impl ExprReturn {}
