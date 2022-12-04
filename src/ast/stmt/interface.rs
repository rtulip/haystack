use std::collections::HashMap;

use crate::{
    ast::arg::UntypedArg,
    error::HayError,
    lex::token::Token,
    types::{validate_requirements, InterfaceBaseType, Type, TypeId, TypeMap},
};

use super::{FunctionStmt, FunctionStubStmt, GlobalEnv, StmtKind};

#[derive(Clone)]
pub struct InterfaceStmt {
    pub token: Token,
    pub name: Token,
    pub annotations: Vec<UntypedArg>,
    pub types: HashMap<TypeId, Token>,
    pub stubs: Vec<FunctionStubStmt>,
    pub fns: Vec<FunctionStmt>,
    pub requires: Option<Vec<Token>>,
}

impl InterfaceStmt {
    pub fn add_to_global_scope(
        self,
        types: &mut TypeMap,
        global_env: &mut GlobalEnv,
    ) -> Result<(), HayError> {
        let mut fns: Vec<String> = self.fns.iter().map(|f| f.name.lexeme.clone()).collect();

        for s in &self.stubs {
            fns.push(s.name.lexeme.clone());
        }

        if let Some(requirements) = &self.requires {
            validate_requirements(requirements, types)?;
        }

        let typ = Type::InterfaceBase(InterfaceBaseType {
            token: self.token,
            name: self.name.clone(),
            annotations: self
                .annotations
                .into_iter()
                .map(|arg| TypeId::new(arg.token.lexeme))
                .collect(),
            types: self.types,
            fns,
            requires: self.requires,
            impls: vec![],
        });
        let tid = typ.id();

        if types.insert(tid.clone(), typ).is_some() {
            return Err(HayError::new(
                format!(
                    "Interface name conflict: `{}` defined elsewhere.",
                    self.name.lexeme
                ),
                self.name.loc,
            ));
        }

        for func in self.fns {
            func.add_to_global_scope(
                types,
                global_env,
                Some(&tid),
                StmtKind::InterfaceFunction(tid.clone()),
            )?;
        }

        for stub in self.stubs {
            stub.add_to_global_scope(
                types,
                global_env,
                Some(&tid),
                StmtKind::InterfaceFunction(tid.clone()),
            )?;
        }

        Ok(())
    }
}
