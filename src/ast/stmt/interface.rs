use std::collections::HashMap;

use crate::{
    ast::arg::UntypedArg,
    backend::{InitDataMap, UninitDataMap},
    error::HayError,
    lex::token::Token,
    types::{TypeId, TypeMap},
};

use super::{GlobalEnv, Stmt};

#[derive(Clone)]
pub struct InterfaceStmt {
    pub token: Token,
    pub name: Token,
    pub annotations: Vec<UntypedArg>,
    pub types: HashMap<TypeId, Token>,
    pub fns: Vec<Stmt>,
}

impl InterfaceStmt {
    pub fn add_to_global_scope(
        self,
        _types: &mut TypeMap,
        _global_env: &mut GlobalEnv,
        _init_data: &mut InitDataMap,
        _uninit_data: &mut UninitDataMap,
    ) -> Result<(), HayError> {
        unimplemented!()
    }
}
