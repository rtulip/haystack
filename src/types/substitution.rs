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
                        match self.insert(k, v.clone()) {
                            Some(ty) => {
                                if ty != v {
                                    return Err(UnificationError::TypesNotEqual(v, ty));
                                }
                            }
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

#[cfg(test)]
mod tests {
    use crate::types::{Ty, UnificationError};

    use super::Substitution;

    #[test]
    fn unify_subs() {
        let s1 = Substitution::from([(0.into(), Ty::var(1))]);
        let s2 = Substitution::from([(0.into(), Ty::Str)]);

        assert_eq!(
            s1.clone().unify(s2.clone()).unwrap(),
            Substitution::from([(0.into(), Ty::var(1)), (1.into(), Ty::Str)])
        );

        assert_eq!(
            s2.unify(s1).unwrap(),
            Substitution::from([(0.into(), Ty::Str), (1.into(), Ty::Str)])
        );

        let s1 = Substitution::from([(1.into(), Ty::var(2)), (2.into(), Ty::Str)]);
        let s2 = Substitution::from([(1.into(), Ty::U32)]);

        assert_eq!(
            s1.unify(s2),
            Err(UnificationError::TypesNotEqual(Ty::U32, Ty::Str))
        );

        let s1 = Substitution::from([(1.into(), Ty::var(2)), (2.into(), Ty::Str)]);
        let s2 = Substitution::from([(1.into(), Ty::Str)]);

        assert_eq!(
            s1.unify(s2).unwrap(),
            Substitution::from([(2.into(), Ty::Str), (1.into(), Ty::var(2))])
        );
    }
}
