use std::collections::HashMap;

use crate::{
    ast::expr::VarExpr,
    // backend::{InitData, UninitData},
    error::HayError,
    lex::token::Token,
    types::TypeId,
};

use super::StmtKind;

#[derive(Debug, Clone)]
pub struct VarStmt {
    pub token: Token,
    pub expr: VarExpr,
}

impl VarStmt {}
