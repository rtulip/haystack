use std::collections::{hash_map::IntoIter, HashMap};
use std::convert::From;

use super::{Ty, TyVar, UnificationError, Variance};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Substitution<'a>(HashMap<TyVar, Ty<'a>>);
impl<'a> Substitution<'a> {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn unify(mut self, other: Self) -> Result<Self, UnificationError<'a>> {
        for (k, v) in other.into_iter() {
            match self.get(&k) {
                Some(ty) => {
                    let sub = ty.clone().unify(v, Variance::Covariant)?;
                    for (k, v) in sub {
                        match self.insert(k, v) {
                            Some(_) => todo!(),
                            None => (),
                        }
                    }
                }
                None => assert!(self.insert(k, v).is_none()),
            }
        }

        Ok(self)
    }

    pub fn get(&self, k: &TyVar) -> Option<&Ty<'a>> {
        self.0.get(k)
    }

    fn insert(&mut self, k: TyVar, v: Ty<'a>) -> Option<Ty<'a>> {
        self.0.insert(k, v)
    }
}

impl<'a, const N: usize> From<[(TyVar, Ty<'a>); N]> for Substitution<'a> {
    fn from(value: [(TyVar, Ty<'a>); N]) -> Self {
        Self(HashMap::from(value))
    }
}

impl<'a> IntoIterator for Substitution<'a> {
    type Item = (TyVar, Ty<'a>);
    type IntoIter = IntoIter<TyVar, Ty<'a>>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> FromIterator<(TyVar, Ty<'a>)> for Substitution<'a> {
    fn from_iter<T: IntoIterator<Item = (TyVar, Ty<'a>)>>(iter: T) -> Self {
        Self(HashMap::from_iter(iter))
    }
}
