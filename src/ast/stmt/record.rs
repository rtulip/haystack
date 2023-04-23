use std::collections::HashSet;

use crate::{
    ast::{arg::UntypedArg, member::UntypedMember},
    error::HayError,
    lex::token::Token,
    types::{RecordKind, Type, TypeId},
};

use super::Stmt;

#[derive(Debug, Clone)]
pub struct RecordStmt {
    pub token: Token,
    pub name: Token,
    pub annotations: Option<Vec<UntypedArg>>,
    pub members: Vec<UntypedMember>,
    pub kind: RecordKind,
    pub requires: Option<Vec<Token>>,
}

impl RecordStmt {}
