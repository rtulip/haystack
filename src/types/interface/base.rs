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

#[derive(Debug, Clone)]
pub struct InterfaceInstanceType {
    pub token: Token,
    pub base: TypeId,
    pub mapping: Vec<TypeId>,
    pub types: Vec<TypeId>,
    pub fns_map: HashMap<TypeId, TypeId>,
    pub generics: Option<Vec<TypeId>>,
    pub requires: Option<Vec<Token>>,
}

impl InterfaceInstanceType {
    pub fn id(&self) -> TypeId {
        let mut name = format!("{}<", self.token.lexeme);
        for t in &self.mapping[0..self.mapping.len() - 1] {
            name = format!("{name}{t} ");
        }
        name = format!("{name}{}>", self.mapping.last().unwrap());

        TypeId::new(name)
    }

    pub fn assign(
        mut self,
        token: &Token,
        func: &TypeId,
        map: &HashMap<TypeId, TypeId>,
        types: &mut TypeMap,
        global_env: &mut GlobalEnv,
    ) -> Result<String, HayError> {
        if self.generics.is_none() {
            return Ok(self.fns_map.get(func).unwrap().0.clone());
        }

        if let Some(requirements) = &self.requires {
            match check_requirements(&self.token, requirements, types, map) {
                Err((Some(req), e)) => {
                    return Err(HayError::new(
                        format!(
                            "Blanket implementation of {} cannot use mapping {map:?}",
                            self.id()
                        ),
                        token.loc.clone(),
                    )
                    .with_hint("Requirements are not satisfied:")
                    .with_hint(format!(
                        "Implementation `{}` requires `{}` is implemented",
                        self.id(),
                        req.lexeme
                    ))
                    .with_hint(e.message()))
                }
                Err((_, e)) => return Err(e),
                Ok(_) => (),
            }
        }

        for t in self.mapping.iter_mut() {
            *t = t.assign(&self.token, map, types)?;
        }

        if types.contains_key(&self.id()) {
            return Err(HayError::new(
                format!(
                    "Interface {} already exists. Don't need to insert into type map",
                    self.id()
                ),
                token.loc.clone(),
            ));
        }

        for func in self.fns_map.values_mut() {
            let (kind, mut sig) = global_env.get(&func.0).unwrap().clone();

            let annotations = &sig
                .generics
                .as_ref()
                .unwrap_or_else(|| panic!("Signature should be generic: {func} {sig:?}"))
                .iter()
                .map(|t| map.get(t).unwrap().clone())
                .collect::<Vec<TypeId>>();

            sig.assign(&self.token, annotations, types)?;
            *func = func.assign(&self.token, map, types)?;
            global_env.insert(func.0.clone(), (kind, sig));
        }

        self.generics = None;
        self.requires = None;

        let tid = self.id();
        let mapped_fn = self.fns_map.get(func).unwrap().0.clone();

        match types.get_mut(&self.base).unwrap() {
            Type::InterfaceBase(base) => base.impls.push(tid.clone()),
            _ => unreachable!(),
        }
        types.insert(tid, Type::InterfaceInstance(self));

        Ok(mapped_fn)
    }
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
                format!("Uncrecognized associated type {typ}"),
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
