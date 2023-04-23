use crate::{
    ast::arg::UntypedArg,
    error::HayError,
    lex::token::Token,
    types::{RecordKind, Type, TypeId},
};

use super::Stmt;

#[derive(Clone)]
pub struct PreDeclarationStmt {
    pub token: Token,
    pub name: Token,
    pub kind: RecordKind,
    pub annotations: Option<Vec<UntypedArg>>,
}

impl PreDeclarationStmt {}
