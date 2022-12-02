use crate::{
    ast::arg::UntypedArg,
    backend::{InitDataMap, UninitDataMap},
    error::HayError,
    lex::token::Token,
    types::{FnTag, TypeMap},
};

use super::GlobalEnv;

#[derive(Debug, Clone)]
pub struct FunctionStubStmt {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<UntypedArg>,
    pub outputs: Vec<UntypedArg>,
    pub annotations: Option<Vec<UntypedArg>>,
    pub impl_on: Option<Token>,
    pub tags: Vec<FnTag>,
}

impl FunctionStubStmt {
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
