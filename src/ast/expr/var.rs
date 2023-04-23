use crate::{error::HayError, lex::token::Token};

#[derive(Debug, Clone)]
pub struct ExprVar {
    /// The token of the `var` keyword
    pub token: Token,
    /// The token of the type of the var
    pub typ: Token,
    /// The token of the name of the var.
    pub ident: Token,
}

impl ExprVar {}
