use crate::{
    ast::arg::UntypedArg,
    error::HayError,
    lex::token::Token,
    types::{Type, TypeId},
};

use super::{FnTag, Stmt, StmtKind};

#[derive(Debug, Clone)]
pub struct FunctionStubStmt {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<UntypedArg>,
    pub outputs: Vec<UntypedArg>,
    pub annotations: Option<Vec<UntypedArg>>,
    pub impl_on: Option<Token>,
    pub tags: Vec<FnTag>,
    pub requires: Option<Vec<Token>>,
}

impl FunctionStubStmt {}
