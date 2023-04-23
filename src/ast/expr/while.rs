use std::collections::HashMap;

use crate::{ast::stmt::StmtKind, error::HayError, lex::token::Token};

use super::Expr;

#[derive(Debug, Clone)]
pub struct ExprWhile {
    /// The token of the `while` keyword
    pub token: Token,
    /// The condition expressions before the body
    pub cond: Vec<Expr>,
    /// The body of the `while` loop.
    pub body: Box<Expr>,
}

impl ExprWhile {}
