use crate::{error::HayError, lex::token::Token, types::Type};

#[derive(Debug, Clone)]
pub struct NeverExpr {
    pub token: Token,
}

impl NeverExpr {}
