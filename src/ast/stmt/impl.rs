use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        arg::{TypedArg, UntypedArg},
        member::UntypedMember,
        stmt::StmtKind,
    },
    error::HayError,
    lex::token::{Token, TokenKind, TypeToken},
};

use super::{FunctionStmt, Stmt};

#[derive(Clone)]
pub struct InterfaceImplStmt {
    pub token: Token,
    pub interface: Token,
    pub types: Vec<UntypedMember>,
    pub fns: Vec<FunctionStmt>,
    pub generics: Option<Vec<UntypedArg>>,
    pub requires: Option<Vec<Token>>,
}

impl InterfaceImplStmt {
    fn is_generic(&self) -> bool {
        self.generics.is_some()
    }
}
