use std::collections::HashMap;

use crate::{error::HayError, lex::token::Token};

use super::Expr;

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub open: Token,
    pub close: Token,
    pub exprs: Vec<Expr>,
}
