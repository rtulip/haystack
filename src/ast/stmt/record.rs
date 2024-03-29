use std::collections::HashSet;

use crate::{
    ast::{arg::UntypedArg, member::UntypedMember},
    error::HayError,
    lex::token::Token,
    types::{validate_requirements, RecordKind, Type, TypeId, TypeMap, VariantType},
};

use super::Stmt;

#[derive(Debug, Clone)]
pub struct RecordStmt {
    pub token: Token,
    pub name: Token,
    pub annotations: Option<Vec<UntypedArg>>,
    pub members: Vec<UntypedMember>,
    pub kind: RecordKind,
    pub requires: Option<Vec<Token>>,
}

impl RecordStmt {
    pub fn add_to_global_scope(self, types: &mut TypeMap) -> Result<(), HayError> {
        let generics = Stmt::bulid_local_generics(self.annotations, types, None)?;
        let members = UntypedMember::resolve(self.members, types, &generics)?;

        if let Some(requirements) = &self.requires {
            validate_requirements(requirements, types)?;
        }

        let tid = TypeId::new(&self.name.lexeme);

        if self.kind == RecordKind::EnumStruct {
            for m in &members {
                let t = Type::Variant(VariantType {
                    base: tid.clone(),
                    variant: m.ident.lexeme.clone(),
                });
                types.insert(t.id(), t);
            }
        }

        let prev = match generics.len() {
            0 => {
                if self.requires.is_some() {
                    return Err(HayError::new(
                        format!(
                            "Cannot have interface requirements on a non-generic {}",
                            self.kind
                        ),
                        self.requires.unwrap().first().unwrap().loc.clone(),
                    ));
                }

                types.insert(
                    tid.clone(),
                    Type::Record {
                        token: self.token.clone(),
                        name: self.name.clone(),
                        members,
                        kind: self.kind,
                        parent: None,
                    },
                )
            }
            _ => types.insert(
                tid.clone(),
                Type::GenericRecordBase {
                    token: self.token.clone(),
                    name: self.name.clone(),
                    generics: generics.clone(),
                    members,
                    kind: self.kind,
                    requires: self.requires,
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

        tid.check_recursive(&self.name, types, &mut HashSet::new())?;

        Ok(())
    }
}
