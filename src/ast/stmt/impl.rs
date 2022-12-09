use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        arg::{TypedArg, UntypedArg},
        member::UntypedMember,
        stmt::StmtKind,
    },
    error::HayError,
    lex::token::{Token, TokenKind, TypeToken},
    types::{
        check_requirements, GenericFunction, InterfaceInstanceType, Type, TypeId, TypeMap,
        UncheckedFunction,
    },
};

use super::{FunctionStmt, GlobalEnv, Stmt};

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
    fn is_generic(&self) -> bool {
        self.generics.is_some()
    }

    pub fn add_to_global_scope(
        self,
        types: &mut TypeMap,
        global_env: &mut GlobalEnv,
    ) -> Result<(), HayError> {
        let is_generic = self.is_generic();
        let (base, inner) = match &self.interface.kind {
            TokenKind::Type(TypeToken::Parameterized { base, inner }) => {
                let mut inner_tids = vec![];
                for t in inner {
                    inner_tids.push(TypeId::from_type_token(
                        &self.interface,
                        t,
                        types,
                        &if let Some(generics) = &self.generics {
                            generics
                                .iter()
                                .map(|arg| TypeId::new(&arg.token.lexeme))
                                .collect()
                        } else {
                            vec![]
                        },
                    )?);
                }
                (base, inner_tids)
            }
            _ => unreachable!(),
        };

        let interface_tid = TypeId::new(base);
        let interface = match types.get(&interface_tid) {
            Some(Type::InterfaceBase(base)) => base.clone(),
            Some(_) => {
                return Err(HayError::new(
                    format!("Cannot implement `{interface_tid}`, as it is not an interface"),
                    self.interface.loc,
                ))
            }
            None => {
                return Err(HayError::new(
                    format!("Unrecognized interface: `{interface_tid}`"),
                    self.interface.loc,
                ))
            }
        };

        if inner.len() != interface.annotations.len() {
            return Err(HayError::new(
                format!("Incorrect number of annotations for interface `{interface_tid}`"),
                self.interface.loc,
            )
            .with_hint(format!(
                "Expected annotations for: {:?}",
                interface.annotations
            ))
            .with_hint(format!("Found annotations:        {inner:?}")));
        }
        // Create a mapping from each interface generic to a type
        let mut map: HashMap<TypeId, TypeId> = HashMap::from_iter(
            interface
                .annotations
                .clone()
                .into_iter()
                .zip(inner.into_iter()),
        );

        // Create a mapping for each associated type
        let mut to_define: HashSet<&TypeId> = HashSet::new();
        for (key, _) in &interface.types {
            to_define.insert(key);
        }

        let local_generics = Stmt::bulid_local_generics(self.generics.clone(), types, None)?;

        for t in self.types {
            let tid = TypeId::new(t.ident.lexeme);
            let typ = TypeId::from_token(&t.token, types, &local_generics)?;
            if to_define.remove(&tid) {
                map.insert(tid, typ);
            } else if map.contains_key(&tid) {
                return Err(HayError::new(
                    format!("Associated type `{tid}` defined multiple times."),
                    t.token.loc,
                ));
            } else {
                return Err(HayError::new(
                    format!("Unrecognized associated type: `{tid}`"),
                    t.token.loc,
                )
                .with_hint(format!(
                    "Interface `{interface_tid}` expects the following associated types:",
                ))
                .with_hint(format!(
                    "{:?}",
                    interface
                        .types
                        .iter()
                        .map(|(k, _)| k)
                        .collect::<Vec<&TypeId>>()
                )));
            }
        }

        if !to_define.is_empty() {
            let mut err = HayError::new("Missing interface associated types.", self.interface.loc)
                .with_hint(format!(
                    "The following types were not defined for interface `{interface_tid}`:"
                ));

            for tid in to_define {
                err = err.with_hint(format!(" * _: {tid}"));
            }

            return Err(err);
        }

        // Check that another implementation with the same __Annotations__ isn't already implemented.
        if let Ok(reimpl) = interface.find_impl(&self.interface, &map) {
            return Err(HayError::new(
                format!("Interface {reimpl} has already been implemented"),
                self.interface.loc,
            ));
        }

        if let Some(requirements) = &interface.requires {
            match check_requirements(&self.interface, requirements, types, &map) {
                Err((Some(r), e)) => {
                    return Err(HayError::new(
                        format!("Failed to implement Interface `{}`", self.interface.lexeme),
                        self.interface.loc,
                    )
                    .with_hint(format!(
                        "Interface `{}` requires `{}` is implemented",
                        interface.name.lexeme, r.lexeme
                    ))
                    .with_hint(e.message()));
                }
                Err((_, e)) => return Err(e),
                _ => (),
            }
        }

        // Assign that mapping to each interface signature & ensure the signatures match.
        let mut to_define = HashSet::new();
        for f in &interface.fns {
            to_define.insert(f);
        }

        let mut fns_map = HashMap::new();
        let mut mapped = vec![];
        for ann in &interface.annotations {
            mapped.push(map.get(ann).unwrap().clone());
        }
        for (typ, _) in &interface.types {
            mapped.push(map.get(typ).unwrap().clone());
        }

        for mut f in self.fns {
            match to_define.take(&f.name.lexeme) {
                Some(func) => {
                    let mut new_fn_name = format!("{}<", f.name.lexeme);
                    for t in &mapped[0..mapped.len() - 1] {
                        new_fn_name = format!("{new_fn_name}{t} ");
                    }
                    new_fn_name = format!("{new_fn_name}{}>", mapped.last().unwrap());

                    f.name.lexeme = new_fn_name.clone();
                    f.annotations = self.generics.clone();

                    let tok = f.name.clone();

                    // Insert the concrete functions renamed
                    f.add_to_global_scope(types, global_env, None, StmtKind::Function)?;

                    let (_, mut interface_sig) = global_env.get(func).unwrap().clone();
                    interface_sig.assign(&tok, &mapped, types)?;

                    match global_env.get(&new_fn_name) {
                        Some((StmtKind::Function, fn_sig)) => {
                            if interface_sig.inputs.len() != fn_sig.inputs.len()
                                || interface_sig.outputs.len() != fn_sig.outputs.len()
                                || interface_sig
                                    .inputs
                                    .iter()
                                    .zip(fn_sig.inputs.iter())
                                    .any(|(a, b)| a != b)
                                || interface_sig
                                    .outputs
                                    .iter()
                                    .zip(fn_sig.outputs.iter())
                                    .any(|(a, b)| a != b)
                            {
                                return Err(HayError::new(
                                    "Incorrect interface function signature",
                                    tok.loc,
                                )
                                .with_hint(format!("For interface function `{func}`"))
                                .with_hint(format!("Expected: {interface_sig:?}"))
                                .with_hint(format!("Found   : {fn_sig:?}")));
                            }
                        }
                        _ => unreachable!(),
                    }

                    fns_map.insert(TypeId::new(func), TypeId::new(new_fn_name));
                }
                None => {
                    let mut err = HayError::new(
                        format!("Unexpected interface function: `{}`", f.name.lexeme),
                        f.token.loc.clone(),
                    )
                    .with_hint(format!(
                        "Interface `{interface_tid}` defines the following functions:",
                    ));
                    for f in &interface.fns {
                        err = err.with_hint(format!(" * fn {f}"));
                    }

                    return Err(err);
                }
            }
        }

        let mut missing = HashSet::new();
        for f in to_define {
            let tid = TypeId::new(f);
            match types.get(&tid) {
                Some(Type::GenericFunction { func }) => {
                    let mut func = func.clone();
                    let mut new_fn_name = format!("{f}<");
                    for t in &mapped[0..mapped.len() - 1] {
                        new_fn_name = format!("{new_fn_name}{t} ");
                    }
                    new_fn_name = format!("{new_fn_name}{}>", mapped.last().unwrap());

                    func.name.lexeme = new_fn_name.clone();
                    let (_, mut interface_sig) = global_env.get(f).unwrap().clone();
                    interface_sig.assign(&interface.token, &mapped, types)?;
                    if let Some(generics) = &self.generics {
                        interface_sig.generics = Some(
                            generics
                                .iter()
                                .map(|arg| TypeId::new(&arg.token.lexeme))
                                .collect(),
                        );
                    }

                    func.inputs = interface_sig
                        .inputs
                        .iter()
                        .zip(func.inputs.into_iter())
                        .map(|(t, arg)| TypedArg {
                            token: arg.token,
                            mutable: arg.mutable,
                            ident: arg.ident,
                            typ: t.clone(),
                        })
                        .collect();
                    func.outputs = interface_sig
                        .outputs
                        .iter()
                        .zip(func.outputs.into_iter())
                        .map(|(t, arg)| TypedArg {
                            token: arg.token,
                            mutable: arg.mutable,
                            ident: arg.ident,
                            typ: t.clone(),
                        })
                        .collect();

                    let new_tid = if !is_generic {
                        let func = UncheckedFunction {
                            token: func.token,
                            name: func.name,
                            inputs: func.inputs,
                            outputs: func.outputs,
                            body: func.body,
                            generic_map: Some(map.clone()),
                            tags: func.tags,
                            impl_on: func.impl_on,
                        };

                        let new_tid = TypeId::new(&new_fn_name);
                        types.insert(new_tid.clone(), Type::UncheckedFunction { func });
                        new_tid
                    } else {
                        let func = GenericFunction {
                            token: func.token,
                            name: func.name,
                            inputs: func.inputs,
                            outputs: func.outputs,
                            body: func.body,
                            tags: func.tags,
                            impl_on: func.impl_on,
                            generics: self
                                .generics
                                .as_ref()
                                .unwrap()
                                .iter()
                                .map(|arg| TypeId::new(&arg.token.lexeme))
                                .collect(),
                            requires: self.requires.clone(),
                        };

                        let new_tid = TypeId::new(&new_fn_name);
                        types.insert(new_tid.clone(), Type::GenericFunction { func });
                        new_tid
                    };

                    fns_map.insert(tid, new_tid);
                    global_env.insert(new_fn_name, (StmtKind::Function, interface_sig));
                }
                _ => {
                    missing.insert(f);
                }
            }
        }

        if !missing.is_empty() {
            let mut err = HayError::new(
                "Missing interface function implementations.",
                self.interface.loc,
            )
            .with_hint(format!(
                "The following functions were not implemented for interface `{interface_tid}`:"
            ));

            for tid in missing {
                err = err.with_hint(format!(" * fn {tid}"));
            }

            return Err(err);
        }

        let mapping = interface
            .annotations
            .iter()
            .map(|t| map.get(t).unwrap().clone())
            .collect();
        let instance_types = interface
            .types
            .iter()
            .map(|(tid, _)| map.get(tid).unwrap().clone())
            .collect();

        let instance_typ = Type::InterfaceInstance(InterfaceInstanceType {
            token: Token {
                kind: self.interface.kind.clone(),
                lexeme: base.clone(),
                loc: self.interface.loc,
            },
            base: interface_tid.clone(),
            mapping,
            types: instance_types,
            fns_map,
            generics: self.generics.map(|generics| {
                generics
                    .iter()
                    .map(|arg| TypeId::new(&arg.token.lexeme))
                    .collect()
            }),
            requires: self.requires,
        });

        let instance_tid = instance_typ.id();
        types.insert(instance_tid.clone(), instance_typ);
        match types.get_mut(&interface_tid).unwrap() {
            Type::InterfaceBase(base) => {
                base.impls.push(instance_tid);
            }
            _ => unreachable!(),
        }

        Ok(())
    }
}
