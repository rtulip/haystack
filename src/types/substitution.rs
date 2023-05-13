use std::collections::{
    btree_map::{Iter, IterMut},
    BTreeMap, HashMap,
};

use crate::{error::HayError, lex::token::Token};

use super::{Stack, Type, TypeVar};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Substitutions(BTreeMap<TypeVar, Type>);

impl Substitutions {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn empty() -> Self {
        Self(BTreeMap::new())
    }

    pub fn new(token: &Token, free_vars: Vec<TypeVar>, subs: Vec<Type>) -> Result<Self, HayError> {
        if subs.len() != free_vars.len() {
            return Err(HayError::new("Can't make substitution", token.loc.clone()));
        }

        let mut sub_map = BTreeMap::new();

        for (var, typ) in free_vars.into_iter().zip(subs.into_iter()) {
            if let Some(_) = sub_map.insert(var, typ) {
                todo!()
            }
        }

        Ok(Self(sub_map))
    }

    pub fn insert(&mut self, k: TypeVar, v: Type) -> Option<Type> {
        self.0.insert(k, v)
    }

    pub fn get(&self, k: &TypeVar) -> Option<&Type> {
        self.0.get(k)
    }

    pub fn apply(&self, stack: &mut Stack) {
        for t in stack {
            if let Type::TypeVar(var) = t {
                if let Some(sub) = self.get(var) {
                    *t = sub.clone();
                }
            }
        }
    }

    pub fn extend(&mut self, other: Substitutions) {
        self.0.extend(other.0.into_iter())
    }
}

impl<'a> IntoIterator for &'a Substitutions {
    type Item = (&'a TypeVar, &'a Type);
    type IntoIter = Iter<'a, TypeVar, Type>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a> IntoIterator for &'a mut Substitutions {
    type Item = (&'a TypeVar, &'a mut Type);
    type IntoIter = IterMut<'a, TypeVar, Type>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}
