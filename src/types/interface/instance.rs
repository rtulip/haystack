use std::collections::HashMap;

use crate::{
    ast::stmt::GlobalEnv,
    error::HayError,
    lex::token::Token,
    types::{Type, TypeId, TypeMap},
};

use super::base::check_requirements;

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
