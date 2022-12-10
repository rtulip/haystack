use std::collections::HashMap;

use crate::{
    ast::{
        expr::ExprIdent,
        stmt::{GlobalEnv, StmtKind},
    },
    error::HayError,
    lex::token::{Token, TokenKind, TypeToken},
    types::{Stack, Type, TypeId, TypeMap},
};

#[derive(Debug, Clone)]
pub struct InterfaceBaseType {
    pub token: Token,
    pub name: Token,
    pub annotations: Vec<TypeId>,
    pub types: Vec<(TypeId, Token)>,
    pub fns: Vec<String>,
    pub requires: Option<Vec<Token>>,
    pub impls: Vec<TypeId>,
}

impl InterfaceBaseType {
    pub fn id(&self) -> TypeId {
        TypeId::new(&self.name.lexeme)
    }

    pub fn associated_type_id(&self, token: &Token, typ: &TypeId) -> Result<TypeId, HayError> {
        if self.types.iter().any(|(t, _)| t == typ) {
            let tid = TypeId::new(format!("{}::{typ}", self.full_id()));
            Ok(tid)
        } else {
            Err(HayError::new(
                format!("Unrecognized associated type: `{typ}`"),
                token.loc.clone(),
            )
            .with_hint(format!(
                "Interface `{}` has the following associated types:",
                self.id()
            ))
            .with_hint(format!(
                "  {:?}",
                self.types.iter().map(|(t, _)| t).collect::<Vec<&TypeId>>()
            )))
        }
    }

    pub fn full_id(&self) -> TypeId {
        let mut name = format!("{}<", self.name.lexeme);
        for ann in &self.annotations[0..self.annotations.len() - 1] {
            name = format!("{name}{ann} ");
        }
        name = format!("{name}{}>", self.annotations.last().unwrap());
        TypeId::new(name)
    }

    pub fn type_index(&self, token: &Token) -> Result<usize, HayError> {
        if let Some((idx, _)) = self
            .types
            .iter()
            .enumerate()
            .find(|(_, (tid, _))| tid == &TypeId::new(&token.lexeme))
        {
            Ok(idx)
        } else {
            todo!("err")
        }
    }

    pub fn resolve(
        &self,
        expr: &ExprIdent,
        stack: &mut Stack,
        types: &mut TypeMap,
        global_env: &mut GlobalEnv,
    ) -> Result<String, HayError> {
        let mut err = HayError::new_type_err(
            format!(
                "Failed to resolve interface function `{}`",
                &expr.ident.lexeme
            ),
            expr.ident.loc.clone(),
        )
        .with_hint(format!("Interface `{}` is implemented by:", self.full_id(),));

        let fn_tid = TypeId::new(&expr.ident.lexeme);
        for instance in &self.impls {
            let mapped_fn = match types.get(instance) {
                Some(Type::InterfaceInstance(instance)) => {
                    if let Some(requirements) = &instance.requires {
                        let mut msg = format!("  {} where", instance.id());

                        for r in &requirements[0..requirements.len() - 1] {
                            msg = format!("{msg} {},", r.lexeme);
                        }

                        if requirements.len() > 1 {
                            msg = format!(
                                "{msg} and {} are satisfied.",
                                requirements.last().unwrap().lexeme
                            )
                        } else {
                            msg = format!(
                                "{msg} {} is satisfied.",
                                requirements.last().unwrap().lexeme
                            )
                        }

                        err = err.with_hint(msg);
                    } else {
                        err = err.with_hint(format!("  {}", instance.id()));
                    }

                    instance.fns_map.get(&fn_tid).unwrap().clone()
                }
                _ => unreachable!(),
            };

            let stack_before = stack.clone();
            match global_env
                .get(&mapped_fn.0)
                .unwrap_or_else(|| panic!("didn't find function: {mapped_fn}"))
            {
                (StmtKind::Function, signature) => {
                    if let Some(map) = match signature.evaluate(&expr.ident, stack, types) {
                        Ok(map) => map,
                        Err(_) => {
                            *stack = stack_before.clone();
                            continue;
                        }
                    } {
                        let interface = match types.get(instance).unwrap() {
                            Type::InterfaceInstance(instance) => instance.clone(),
                            _ => unreachable!(),
                        };

                        match interface.assign(&expr.ident, &fn_tid, &map, types, global_env) {
                            Ok(mapped_func) => return Ok(mapped_func),
                            Err(_) => *stack = stack_before.clone(),
                        }
                    } else {
                        return Ok(mapped_fn.0);
                    }
                }
                _ => unreachable!(),
            }
        }

        let (_, interface_sig) = global_env.get(&expr.ident.lexeme).unwrap();
        Err(err
            .with_hint(format!("Function `{}` expected:", &expr.ident.lexeme))
            .with_hint(format!("  {:?}", interface_sig.inputs))
            .with_hint("Found:")
            .with_hint(format!(
                "  {:?}",
                stack
                    .iter()
                    .rev()
                    .take(interface_sig.inputs.len())
                    .rev()
                    .collect::<Vec<&TypeId>>()
            )))
    }

    pub fn find_impl(
        &self,
        token: &Token,
        map: &HashMap<TypeId, TypeId>,
    ) -> Result<TypeId, HayError> {
        let mut impl_name = format!("{}<", self.name.lexeme);
        for ann in &self.annotations[0..self.annotations.len() - 1] {
            impl_name = format!(
                "{impl_name}{} ",
                map.get(ann)
                    .expect("Should have found a mapping for annotation")
            );
        }
        impl_name = format!(
            "{impl_name}{}>",
            map.get(self.annotations.last().unwrap())
                .expect("Should have found a mapping for the annotations")
        );

        if self.impls.iter().any(|i| i.0 == impl_name) {
            Ok(TypeId::new(impl_name))
        } else {
            Err(HayError::new(
                format!("Interface `{impl_name}` is not implemented"),
                token.loc.clone(),
            ))
        }
    }
}

pub fn check_requirements<'a>(
    token: &Token,
    requirements: &'a Vec<Token>,
    types: &mut TypeMap,
    map: &HashMap<TypeId, TypeId>,
) -> Result<(), (Option<&'a Token>, HayError)> {
    for req in requirements {
        match &req.kind {
            TokenKind::Type(TypeToken::Parameterized { base, inner }) => {
                let mut inner_map = vec![];
                for t in inner {
                    let inner_tid = match TypeId::from_type_token(
                        req,
                        t,
                        types,
                        &map.keys().cloned().collect(),
                    ) {
                        Ok(t) => t,
                        Err(e) => return Err((None, e)),
                    };
                    let inner_mapped = map.get(&inner_tid).unwrap().clone();
                    inner_map.push(inner_mapped);
                }
                match types.get(&TypeId::new(base)) {
                    Some(Type::InterfaceBase(req_base)) => {
                        let mut aliased_map = HashMap::new();
                        for (t, c) in req_base
                            .annotations
                            .clone()
                            .into_iter()
                            .zip(inner_map.into_iter())
                        {
                            aliased_map.insert(t, c);
                        }

                        if let Err(e) = req_base.find_impl(token, &aliased_map) {
                            return Err((Some(req), e));
                        }
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }
    Ok(())
}

pub fn validate_requirements(requirements: &Vec<Token>, types: &TypeMap) -> Result<(), HayError> {
    for t in requirements {
        match &t.kind {
            TokenKind::Type(TypeToken::Parameterized { base, .. }) => {
                match types.get(&TypeId::new(base)) {
                    Some(Type::InterfaceBase(_)) => (),
                    _ => {
                        return Err(HayError::new(
                            format!("Invalid requirement: `{}` is not an interface.", t.lexeme),
                            t.loc.clone(),
                        ));
                    }
                }
            }
            _ => {
                return Err(HayError::new(
                    format!("Invalid requirement: `{}` is not an interface.", t.lexeme),
                    t.loc.clone(),
                ));
            }
        }
    }

    Ok(())
}
