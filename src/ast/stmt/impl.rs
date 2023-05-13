use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        arg::{TypedArg, UntypedArg},
        member::UntypedMember,
        stmt::{Functions, StmtKind},
    },
    error::HayError,
    lex::token::{Token, TokenKind, TypeToken},
    types::{FreeVars, Type, TypeVar},
};

use super::{FunctionStmt, InterfaceImpl, Interfaces, Stmt, UserDefinedTypes};

#[derive(Clone)]
pub struct InterfaceImplStmt {
    pub token: Token,
    pub interface: Token,
    pub types: Vec<UntypedMember>,
    pub fns: Vec<FunctionStmt>,
    pub generics: Option<Vec<UntypedArg>>,
    pub requires: Option<Vec<Token>>,
}

impl InterfaceImplStmt {
    pub fn add_to_global_env(
        self,
        user_defined_types: &UserDefinedTypes,
        interfaces: &mut Interfaces,
    ) -> Result<(), HayError> {
        let (free_vars, _) = UntypedArg::into_free_vars(self.generics);

        let (base, inner) = match &self.interface.kind {
            TokenKind::Type(TypeToken::Parameterized { base, inner }) => {
                let mut inner_tids = vec![];
                for t in inner {
                    inner_tids.push(Type::from_type_token(
                        &self.interface,
                        t,
                        user_defined_types,
                        interfaces,
                        free_vars.as_ref(),
                    )?);
                }
                (base, inner_tids)
            }
            _ => unreachable!(),
        };

        let interface = match interfaces.get(base) {
            Some(interface) => interface,
            None => {
                return Err(HayError::new(
                    format!("Unrecognized interface: `{base}`"),
                    self.interface.loc,
                ))
            }
        };

        let mut subs = interface.new_substitutions(&self.token, inner)?;

        // Create a mapping for each associated type
        let mut to_define = interface.associated_types.clone();

        for t in self.types {
            let member = t.into_typed_member(user_defined_types, interfaces, free_vars.as_ref())?;

            let type_var = TypeVar::new(member.ident.lexeme);
            if to_define.remove(&type_var) {
                if let Some(_) = subs.insert(type_var, member.typ) {
                    todo!()
                }
            } else {
                todo!()
            }

            // if to_define.remove(&tid) {
            //     map.insert(tid, typ);
            // } else if map.contains_key(&tid) {
            //     return Err(HayError::new(
            //         format!("Associated type `{tid}` defined multiple times."),
            //         t.token.loc,
            //     ));
            // } else {
            //     return Err(HayError::new(
            //         format!("Unrecognized associated type: `{tid}`"),
            //         t.token.loc,
            //     )
            //     .with_hint(format!(
            //         "Interface `{interface_tid}` expects the following associated types:",
            //     ))
            //     .with_hint(format!(
            //         "{:?}",
            //         interface
            //             .types
            //             .iter()
            //             .map(|(k, _)| k)
            //             .collect::<Vec<&TypeId>>()
            //     )));
            // }
        }

        if !to_define.is_empty() {
            let mut err = HayError::new("Missing interface associated types.", self.interface.loc)
                .with_hint(format!(
                    "The following types were not defined for interface `{base}`:"
                ));

            for tid in to_define {
                err = err.with_hint(format!(" * _: {}", tid.0));
            }

            return Err(err);
        }

        // Check that another implementation with the same __Annotations__ isn't already implemented.
        // FIXME!
        // if let Ok(reimpl) = interface.find_impl(&self.interface, &subs) {
        //     return Err(HayError::new(
        //         format!("Interface {reimpl} has already been implemented"),
        //         self.interface.loc,
        //     ));
        // }

        if let Some(requirements) = &interface.requires {
            todo!()
            // match check_requirements(&self.interface, requirements, types, &map) {
            //     Err((Some(r), e)) => {
            //         return Err(HayError::new(
            //             format!("Failed to implement Interface `{}`", self.interface.lexeme),
            //             self.interface.loc,
            //         )
            //         .with_hint(format!(
            //             "Interface `{}` requires `{}` is implemented",
            //             interface.name.lexeme, r.lexeme
            //         ))
            //         .with_hint(e.message()));
            //     }
            //     Err((_, e)) => return Err(e),
            //     _ => (),
            // }
        }

        // Assign that mapping to each interface signature & ensure the signatures match.
        let mut to_define = HashSet::new();
        for (func_id, _) in &interface.functions {
            to_define.insert(func_id.clone());
        }

        let mut functions = Functions::new();
        for f in self.fns {
            match to_define.take(&f.name.lexeme) {
                Some(_) => {
                    // let mut new_fn_name = format!("{}<", f.name.lexeme);
                    // for t in &mapped[0..mapped.len() - 1] {
                    //     new_fn_name = format!("{new_fn_name}{t} ");
                    // }
                    // new_fn_name = format!("{new_fn_name}{}>", mapped.last().unwrap());

                    // f.name.lexeme = new_fn_name.clone();
                    // f.annotations = self.generics.clone();

                    // Insert the concrete functions renamed
                    f.add_to_global_env(
                        user_defined_types,
                        interfaces,
                        &mut functions,
                        free_vars.as_ref(),
                    )?;
                }
                None => {
                    let mut err = HayError::new(
                        format!("Unexpected interface function: `{}`", f.name.lexeme),
                        f.token.loc.clone(),
                    )
                    .with_hint(format!(
                        "Interface `{base}` defines the following functions:",
                    ));
                    for (f, _) in &interface.functions {
                        err = err.with_hint(format!(" * fn {f}"));
                    }

                    return Err(err);
                }
            }
        }

        // let mut missing = HashSet::new();
        for f in to_define {
            let func = interface.functions.get(&f).cloned().unwrap();
            functions.insert(f, func);
        }

        // if !missing.is_empty() {
        //     let mut err = HayError::new(
        //         "Missing interface function implementations.",
        //         self.interface.loc,
        //     )
        //     .with_hint(format!(
        //         "The following functions were not implemented for interface `{interface_tid}`:"
        //     ));

        //     for tid in missing {
        //         err = err.with_hint(format!(" * fn {tid}"));
        //     }

        //     return Err(err);
        // }

        let instance = InterfaceImpl {
            token: self.token.clone(),
            subs,
            functions,
            requires: self.requires.clone(),
            free_vars,
        };

        interfaces.get_mut(base).unwrap().impls.push(instance);

        Ok(())
    }
}
