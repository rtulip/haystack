use std::collections::HashMap;

use crate::{ast::stmt::StmtKind, error::HayError, lex::token::Token};

use super::Expr;

#[derive(Debug, Clone)]
pub struct ExprIf {
    /// Token of the `If` keyword
    pub token: Token,
    /// A list of expressions to execute if true.
    pub then: Box<Expr>,
    /// A list of expressions for each else-if case
    pub otherwise: Vec<ExprElseIf>,
    /// An optional final `else` case.
    pub finally: Option<Box<Expr>>,
}

impl ExprIf {}

#[derive(Debug, Clone)]
pub struct ExprElseIf {
    /// Token of the `else` keyword
    pub token: Token,
    /// The expressions to evaluate before the next `if`.
    pub condition: Vec<Expr>,
    /// The body of the `else` expression
    pub block: Expr,
}

impl ExprElseIf {}
