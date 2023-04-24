use crate::{
    error::HayError,
    lex::token::{Keyword, Operator},
};

use super::{AccessorExpr, Expr, IdentExpr, OperatorExpr};

#[derive(Debug, Clone)]
pub struct ExprUnary {
    pub op: OperatorExpr,
    pub expr: Box<Expr>,
}

impl ExprUnary {}
