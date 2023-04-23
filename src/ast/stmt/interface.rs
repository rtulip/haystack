use crate::{ast::arg::UntypedArg, error::HayError, lex::token::Token, types::TypeId};

use super::{FunctionStmt, FunctionStubStmt, StmtKind};

#[derive(Debug, Clone)]
pub struct InterfaceId(String);

impl InterfaceId {
    pub fn new<S: Into<String>>(s: S) -> Self {
        InterfaceId(s.into())
    }
}

#[derive(Clone)]
pub struct InterfaceStmt {
    pub token: Token,
    pub name: Token,
    pub annotations: Vec<UntypedArg>,
    pub types: Vec<(TypeId, Token)>,
    pub stubs: Vec<FunctionStubStmt>,
    pub fns: Vec<FunctionStmt>,
    pub requires: Option<Vec<Token>>,
}

impl InterfaceStmt {}
