use crate::{
    ast::arg::UntypedArg,
    error::HayError,
    lex::token::Token,
    types::{RecordKind, Type, TypeId, TypeMap},
};

use super::Stmt;

#[derive(Clone)]
pub struct PreDeclarationStmt {
    pub token: Token,
    pub name: Token,
    pub kind: RecordKind,
    pub annotations: Option<Vec<UntypedArg>>,
}

impl PreDeclarationStmt {
    pub fn add_to_global_scope(self, types: &mut TypeMap) -> Result<(), HayError> {
        let tid = TypeId::new(&self.name.lexeme);
        let generics = Stmt::bulid_local_generics(self.annotations, types, None)?;
        match types.get(&tid) {
            Some(Type::Record {
                token: decl_token,
                kind: decl_kind,
                ..
            }) => {
                tid.validate_redeclaration(
                    &self.token,
                    (&self.kind, &self.token, &generics),
                    (decl_kind, decl_token, None),
                )?;
                return Ok(());
            }
            Some(Type::GenericRecordBase {
                token: decl_token,
                kind: decl_kind,
                generics: decl_generics,
                ..
            }) => {
                tid.validate_redeclaration(
                    &self.token,
                    (&self.kind, &self.token, &generics),
                    (decl_kind, decl_token, Some(decl_generics)),
                )?;
                return Ok(());
            }
            Some(Type::RecordPreDeclaration {
                kind: prev_decl_kind,
                generics: prev_decl_generics,
                token: prev_decl_token,
                ..
            }) => {
                if self.kind != *prev_decl_kind || prev_decl_generics.len() != generics.len() {
                    return Err(
                        HayError::new("Conflicting Pre-Declarations.", self.token.loc)
                            .with_hint_and_custom_note(
                                format!("{tid} originally defined here"),
                                format!("{}", &prev_decl_token.loc),
                            ),
                    );
                }
            }
            Some(_) => {
                return Err(HayError::new(
                    format!("Name conflict: `{}` defined elsewhere.", self.name.lexeme),
                    self.name.loc,
                ))
            }
            None => (),
        }

        types.insert(
            tid,
            Type::RecordPreDeclaration {
                token: self.token,
                name: self.name,
                kind: self.kind,
                generics,
            },
        );

        Ok(())
    }
}
