use std::collections::HashSet;

use crate::{
    ast::{arg::UntypedArg, member::UntypedMember},
    error::HayError,
    lex::token::Token,
    types::{RecordKind, RecordMember, RecordType, Type, TypeId},
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

#[derive(Debug)]
pub struct RecordDescription {
    pub token: Token,
    pub id: TypeId,
    pub typ: RecordType,
}

impl RecordStmt {
    pub fn add_to_global_env(
        self,
        user_defined_types: &mut UserDefinedTypes,
        functions: &mut Functions,
    ) -> Result<(), HayError> {
        assert!(self.kind != RecordKind::Tuple);

        let token = self.token;
        let tid = TypeId::new(&self.name.lexeme);
        let typ = RecordType {
            kind: self.kind,
            ident: Some(tid.clone()),
            members: UntypedMember::into_typed_members(self.members, &user_defined_types)?
                .into_iter()
                .map(|m| RecordMember {
                    ident: m.ident.lexeme,
                    vis: m.vis,
                    typ: m.typ,
                })
                .collect(),
        };

        if let Some(prev) = user_defined_types.insert(
            tid.clone(),
            RecordDescription {
                token,
                id: tid,
                typ,
            },
        ) {
            todo!("Err: {prev:?}")
        }

        Ok(())
    }
}
