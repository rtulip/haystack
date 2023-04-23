use crate::{
    ast::stmt::StmtKind,
    error::HayError,
    lex::token::{Operator, Token},
};

#[derive(Debug, Clone)]
pub struct ExprOperator {
    pub op: Operator,
    pub token: Token,
}

impl ExprOperator {}
