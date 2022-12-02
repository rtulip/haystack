use crate::{
    ast::{arg::UntypedArg, member::UntypedMember},
    error::HayError,
    lex::token::Token,
    types::{RecordKind, Type, TypeId, TypeMap},
};

use super::Stmt;

#[derive(Debug, Clone)]
pub struct RecordStmt {
    pub token: Token,
    pub name: Token,
    pub annotations: Option<Vec<UntypedArg>>,
    pub members: Vec<UntypedMember>,
    pub kind: RecordKind,
}

impl RecordStmt {
    pub fn add_to_global_scope(self, types: &mut TypeMap) -> Result<(), HayError> {
        let generics = Stmt::bulid_local_generics(self.annotations, types)?;
        let members = UntypedMember::resolve(self.members, types, &generics)?;

        let prev = match generics.len() {
            0 => types.insert(
                TypeId::new(&self.name.lexeme),
                Type::Record {
                    token: self.token.clone(),
                    name: self.name.clone(),
                    members,
                    kind: self.kind,
                },
            ),
            _ => types.insert(
                TypeId::new(&self.name.lexeme),
                Type::GenericRecordBase {
                    token: self.token.clone(),
                    name: self.name.clone(),
                    generics: generics.clone(),
                    members,
                    kind: self.kind,
                },
            ),
        };

        match prev {
            None => (),
            Some(Type::RecordPreDeclaration {
                token: pre_decl_token,
                kind: pre_decl_kind,
                generics: pre_decl_generics,
                ..
            }) => {
                TypeId::new(&self.name.lexeme).validate_redeclaration(
                    &self.token,
                    (&pre_decl_kind, &pre_decl_token, &pre_decl_generics),
                    (
                        &self.kind,
                        &self.token,
                        if generics.is_empty() {
                            None
                        } else {
                            Some(&generics)
                        },
                    ),
                )?;
            }
            Some(_) => {
                return Err(HayError::new(
                    format!("Name conflict: `{}` defined elsewhere.", self.name.lexeme),
                    self.name.loc,
                ))
            }
        }

        Ok(())
    }
}
