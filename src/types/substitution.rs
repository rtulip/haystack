use std::collections::HashMap;

use crate::{error::HayError, lex::token::Token};

use super::{Type, TypeVar};

#[derive(Debug, Clone)]
pub struct Substitutions(HashMap<TypeVar, Type>);

impl Substitutions {
    pub fn empty() -> Self {
        Self(HashMap::new())
    }

    pub fn new(token: &Token, free_vars: Vec<TypeVar>, subs: Vec<Type>) -> Result<Self, HayError> {
        if subs.len() != free_vars.len() {
            return Err(HayError::new("Can't make substitution", token.loc.clone()));
        }

        let mut sub_map = HashMap::new();

        for (var, typ) in free_vars.into_iter().zip(subs.into_iter()) {
            if let Some(_) = sub_map.insert(var, typ) {
                todo!()
            }
        }

        Ok(Self(sub_map))
    }

    pub fn get(&self, k: &TypeVar) -> Option<&Type> {
        self.0.get(k)
    }
}
