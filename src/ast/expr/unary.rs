use crate::{
    error::HayError,
    lex::token::{Keyword, Operator},
};

use super::{AccessorExpr, Expr, ExprIdent, ExprOperator};

#[derive(Debug, Clone)]
pub struct ExprUnary {
    pub op: ExprOperator,
    pub expr: Box<Expr>,
}

impl ExprUnary {}
