use std::collections::HashMap;

use crate::{
    ast::arg::UntypedArg,
    error::HayError,
    lex::token::Token,
    types::{InterfaceBaseType, Type, TypeId, TypeMap},
};

use super::{FunctionStmt, FunctionStubStmt, GlobalEnv};

#[derive(Clone)]
pub struct InterfaceStmt {
    pub token: Token,
    pub name: Token,
    pub annotations: Vec<UntypedArg>,
    pub types: HashMap<TypeId, Token>,
    pub stubs: Vec<FunctionStubStmt>,
    pub fns: Vec<FunctionStmt>,
}

impl InterfaceStmt {
    pub fn add_to_global_scope(
        self,
        types: &mut TypeMap,
        global_env: &mut GlobalEnv,
    ) -> Result<(), HayError> {
        println!("Annotations: {:?}", self.annotations);
        let typ = Type::InterfaceBase(InterfaceBaseType {
            token: self.token,
            name: self.name.clone(),
            annotations: self
                .annotations
                .into_iter()
                .map(|arg| TypeId::new(arg.token.lexeme))
                .collect(),
            types: self.types,
        });
        let tid = typ.id();

        println!("tid: {tid}");

        if let Some(_) = types.insert(tid.clone(), typ) {
            return Err(HayError::new(
                format!(
                    "Interface name conflict: `{}` defined elsewhere.",
                    self.name.lexeme
                ),
                self.name.loc,
            ));
        }

        for func in self.fns {
            func.add_to_global_scope(types, global_env, Some(&tid))?;
        }

        for stub in self.stubs {
            stub.add_to_global_scope(types, global_env, Some(&tid))?;
        }

        Ok(())
    }
}
