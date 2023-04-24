use std::collections::BTreeMap;

use crate::{
    ast::{arg::UntypedArg, stmt::TypeDescription},
    error::HayError,
    lex::token::Token,
    types::{RecordKind, Type, TypeId, TypeVar},
};

use super::{Stmt, UserDefinedTypes};

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
        user_defined_types: &mut UserDefinedTypes,
    ) -> Result<(), HayError> {
        let tid = TypeId::new(&self.name.lexeme);
        assert!(self.annotations.is_none());
        let annotations = None;

        let predecl = PreDeclaredType {
            name: self.name.lexeme,
            kind: self.kind,
            annotations,
        };

        match user_defined_types.get(&tid) {
            Some(TypeDescription::Record(_)) => (),
            Some(TypeDescription::PreDeclaration(pd)) if pd == &predecl => (),
            Some(_) => todo!("Err"),
            None => {
                user_defined_types.insert(tid, TypeDescription::PreDeclaration(predecl));
            }
        }

        Ok(())
    }
}
