use std::collections::HashSet;

use crate::{
    ast::{arg::UntypedArg, member::UntypedMember, stmt::TypeDescription},
    error::HayError,
    lex::token::Token,
    types::{FreeVars, RecordKind, RecordMember, RecordType, Type, TypeId, TypeVar},
};

use super::{FunctionDescription, Functions, Stmt, UserDefinedTypes};

#[derive(Debug, Clone)]
pub struct RecordStmt {
    pub token: Token,
    pub name: Token,
    pub annotations: Option<Vec<UntypedArg>>,
    pub members: Vec<UntypedMember>,
    pub kind: RecordKind,
    pub requires: Option<Vec<Token>>,
}

#[derive(Debug, Clone)]
pub struct RecordDescription {
    pub token: Token,
    pub id: TypeId,
    pub typ: RecordType,
    pub free_vars: Option<Vec<TypeVar>>,
}

impl RecordStmt {
    pub fn add_to_global_env(
        self,
        user_defined_types: &mut UserDefinedTypes,
    ) -> Result<(), HayError> {
        assert!(self.kind != RecordKind::Tuple);

        let (free_vars, ordered_free_vars) = UntypedArg::into_free_vars(self.annotations);
        let token = self.token;
        let tid = TypeId::new(&self.name.lexeme);
        let typ = RecordType {
            kind: self.kind,
            ident: Some(tid.clone()),
            members: UntypedMember::into_typed_members(
                self.members,
                &user_defined_types,
                &free_vars,
            )?
            .into_iter()
            .map(|m| RecordMember {
                ident: m.ident.lexeme,
                vis: m.vis,
                typ: m.typ,
            })
            .collect(),
        };

        if let Some(TypeDescription::Record(prev)) = user_defined_types.insert(
            tid.clone(),
            TypeDescription::Record(RecordDescription {
                token,
                id: tid,
                typ,
                free_vars: ordered_free_vars,
            }),
        ) {
            todo!("Err: {prev:?}")
        }

        Ok(())
    }
}
