use std::collections::HashMap;

use crate::{
    ast::expr::ExprVar,
    // backend::{InitData, UninitData},
    error::HayError,
    lex::token::Token,
    types::TypeId,
};

use super::StmtKind;

#[derive(Debug, Clone)]
pub struct VarStmt {
    pub token: Token,
    pub expr: ExprVar,
}

impl VarStmt {}
