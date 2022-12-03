use std::collections::{HashMap, HashSet};

use crate::{
    ast::{member::UntypedMember, stmt::StmtKind},
    backend::{InitDataMap, UninitDataMap},
    error::HayError,
    lex::token::{Token, TokenKind, TypeToken},
    types::{Type, TypeId, TypeMap},
};

use super::{FunctionStmt, GlobalEnv};

#[derive(Clone)]
pub struct InterfaceImplStmt {
    pub token: Token,
    pub interface: Token,
    pub types: Vec<UntypedMember>,
    pub fns: Vec<FunctionStmt>,
}

impl InterfaceImplStmt {
    pub fn add_to_global_scope(
        self,
        types: &mut TypeMap,
        global_env: &mut GlobalEnv,
        _init_data: &mut InitDataMap,
        _uninit_data: &mut UninitDataMap,
    ) -> Result<(), HayError> {
        let (base, inner) = match &self.interface.kind {
            TokenKind::Type(TypeToken::Parameterized { base, inner }) => (
                base,
                inner
                    .iter()
                    .map(|t| {
                        TypeId::from_type_token(&self.interface, &t, types, &vec![]).expect("TODO!")
                    })
                    .collect::<Vec<TypeId>>(),
            ),
            _ => unimplemented!("Expected paramerterized"),
        };
        let interface_tid = TypeId::new(base);
        let interface = match types.get(&interface_tid) {
            Some(Type::InterfaceBase(base)) => base.clone(),
            Some(_) => unimplemented!("error bad interface type"),
            None => unimplemented!("Unrecognized interface."),
        };

        if inner.len() != interface.annotations.len() {
            todo!("Error")
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
        for key in interface.types.keys() {
            to_define.insert(key);
        }

        for t in self.types {
            let tid = TypeId::new(t.ident.lexeme);
            let typ = TypeId::from_token(&t.token, types, &vec![])?;
            if to_define.remove(&tid) {
                map.insert(tid, typ);
            } else {
                todo!("error")
            }
        }
        if !to_define.is_empty() {
            todo!("Error")
        }

        // Assign that mapping to each interface signature & ensure the signatures match.
        let mut to_define = HashSet::new();
        for f in interface.fns {
            to_define.insert(f);
        }

        for mut f in self.fns {
            match to_define.take(&f.name.lexeme) {
                Some(func) => {
                    let mut mapped = vec![];
                    for ann in &interface.annotations {
                        mapped.push(map.get(ann).unwrap().clone());
                    }
                    for (typ, _) in &interface.types {
                        mapped.push(map.get(typ).unwrap().clone());
                    }

                    let mut new_fn_name = format!("{}<", f.name.lexeme);
                    for t in &mapped[0..mapped.len() - 1] {
                        new_fn_name = format!("{new_fn_name}{t} ");
                    }
                    new_fn_name = format!("{new_fn_name}{}>", mapped.last().unwrap());

                    f.name.lexeme = new_fn_name.clone();

                    let tok = f.name.clone();
                    // Insert the concrete functions renamed
                    f.add_to_global_scope(types, global_env, None)?;

                    let (_, mut interface_sig) = global_env.get(&func).unwrap().clone();
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
                                todo!("Error")
                            }
                        }
                        Some((StmtKind::Var, _)) => todo!("err"),
                        None => todo!("err"),
                    }
                }
                None => todo!("error"),
            }
        }

        if !to_define.is_empty() {
            todo!("error")
        }

        Ok(())
        // TODO: Intsert a InterfaceInstance into types.
    }
}
