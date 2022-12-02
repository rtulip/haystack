use crate::{
    ast::member::UntypedMember,
    backend::{InitDataMap, UninitDataMap},
    error::HayError,
    lex::token::Token,
    types::TypeMap,
};

use super::{GlobalEnv, Stmt};

#[derive(Clone)]
pub struct InterfaceImplStmt {
    pub token: Token,
    pub interface: Token,
    pub types: Vec<UntypedMember>,
    pub fns: Vec<Stmt>,
}

impl InterfaceImplStmt {
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
