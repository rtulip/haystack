use std::collections::HashMap;

use crate::{
    ast::{
        expr::ExprIdent,
        stmt::{GlobalEnv, StmtKind},
    },
    error::HayError,
    lex::token::Token,
    types::signature,
};

use super::{Stack, Type, TypeId, TypeMap};

#[derive(Debug, Clone)]
pub struct InterfaceBaseType {
    pub token: Token,
    pub name: Token,
    pub annotations: Vec<TypeId>,
    pub types: HashMap<TypeId, Token>,
    pub fns: Vec<String>,
    pub impls: Vec<TypeId>,
}

#[derive(Debug, Clone)]
pub struct InterfaceInstanceType {
    pub token: Token,
    pub mapping: Vec<TypeId>,
    pub fns_map: HashMap<TypeId, TypeId>,
}

impl InterfaceBaseType {
    pub fn id(&self) -> TypeId {
        let name = format!("{}", self.name.lexeme);
        TypeId::new(name)
    }

    pub fn resolve(
        &self,
        expr: &ExprIdent,
        stack: &mut Stack,
        types: &mut TypeMap,
        global_env: &GlobalEnv,
    ) -> Result<String, HayError> {
        for instance in &self.impls {
            match types.get(instance) {
                Some(Type::InterfaceInstance(instance)) => {
                    println!("    {}", instance.token);
                    let fn_tid = TypeId::new(&expr.ident.lexeme);
                    let mapped_fn = instance.fns_map.get(&fn_tid).unwrap().clone();
                    drop(instance);
                    match global_env.get(&mapped_fn.0).unwrap() {
                        (StmtKind::Function, signature) => {
                            if signature.evaluate(&expr.ident, stack, types).is_ok() {
                                return Ok(mapped_fn.0);
                            }
                        }
                        _ => todo!("err"),
                    }
                }
                Some(_) => todo!("err"),
                None => todo!("Err"),
            }
        }

        todo!()
    }
}
