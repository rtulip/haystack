use std::collections::HashMap;

use crate::{ast::member::TypedMember, error::HayError, lex::token::Token};

#[derive(Debug, Clone)]
pub struct ExprCast {
    pub token: Token,
    pub typ: Token,
}
