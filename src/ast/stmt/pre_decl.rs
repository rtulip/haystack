use std::collections::BTreeMap;

use crate::{
    ast::arg::UntypedArg,
    error::HayError,
    lex::token::Token,
    types::{RecordKind, Type, TypeId, TypeVar},
};

use super::Stmt;

#[derive(Clone)]
pub struct PreDeclarationStmt {
    pub token: Token,
    pub name: Token,
    pub kind: RecordKind,
    pub annotations: Option<Vec<UntypedArg>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreDeclaredType {
    pub name: String,
    pub kind: RecordKind,
    pub annotations: Option<Vec<TypeVar>>,
}

impl PreDeclarationStmt {
    pub fn add_to_global_env(
        self,
        user_defined_types: &mut BTreeMap<TypeId, Type>,
        pre_declared_types: &mut BTreeMap<TypeId, PreDeclaredType>,
    ) -> Result<(), HayError> {
        let tid = TypeId::new(&self.name.lexeme);
        if user_defined_types.contains_key(&tid) {
            return Ok(());
        }

        assert!(self.annotations.is_none());
        let annotations = None;

        let predecl = PreDeclaredType {
            name: self.name.lexeme,
            kind: self.kind,
            annotations,
        };

        match pre_declared_types.get(&tid) {
            Some(pd) if pd == &predecl => (),
            Some(_) => todo!("Err"),
            None => {
                pre_declared_types.insert(tid, predecl);
            }
        }

        Ok(())
    }
}
