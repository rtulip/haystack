use super::{Substitution, UnificationError, Variance};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyVar(usize);

impl From<usize> for TyVar {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

pub struct TyGen(usize);

impl TyGen {
    pub fn new() -> Self {
        Self(0)
    }

    pub fn fresh<'src>(&mut self) -> Ty<'src> {
        let t = Ty::var(self.0);
        self.0 += 1;
        t
    }

    pub fn fresh_with_var<'src>(&mut self) -> (Ty<'src>, TyVar) {
        let t = Ty::var(self.0);
        let v = self.0.into();
        self.0 += 1;
        (t, v)
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QuantifiedType<'src> {
    ident: &'src str,
    elements: Vec<Ty<'src>>,
}

impl<'src> QuantifiedType<'src> {
    pub fn new<T>(ident: &'src str, ts: T) -> Self
    where
        T: Into<Vec<Ty<'src>>>,
    {
        QuantifiedType {
            ident,
            elements: ts.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty<'src> {
    U32,
    Bool,
    Str,
    Var(TyVar),
    Quant(QuantifiedType<'src>),
}

impl<'src> Ty<'src> {
    pub fn var(n: usize) -> Self {
        Ty::Var(TyVar(n))
    }

    pub fn quantified<T>(ident: &'src str, ts: T) -> Self
    where
        T: Into<Vec<Ty<'src>>>,
    {
        Self::Quant(QuantifiedType::new(ident, ts))
    }

    pub fn substitute(self, subs: &Substitution<'src>) -> Self {
        match self {
            Ty::U32 => Ty::U32,
            Ty::Bool => Ty::Bool,
            Ty::Str => Ty::Str,
            Ty::Var(var) => match subs.get(&var) {
                Some(ty) => ty.clone(),
                None => Ty::Var(var),
            },
            Ty::Quant(QuantifiedType { ident, elements }) => Ty::quantified(
                ident,
                elements
                    .into_iter()
                    .map(|t| t.substitute(subs))
                    .collect::<Vec<_>>(),
            ),
        }
    }

    pub fn unify(
        self,
        other: Self,
        variance: Variance,
    ) -> Result<Substitution<'src>, UnificationError<'src>> {
        match (self, other) {
            (Ty::U32, Ty::U32) => Ok(Substitution::new()),
            (Ty::Str, Ty::Str) => Ok(Substitution::new()),
            (Ty::Bool, Ty::Bool) => Ok(Substitution::new()),
            (Ty::Var(left), Ty::Var(right)) if left == right => Ok(Substitution::new()),
            (Ty::Var(v), t) | (t, Ty::Var(v)) => Ok(Substitution::from([(v, t)])),
            (
                Ty::Quant(QuantifiedType {
                    ident: left,
                    elements: left_elems,
                }),
                Ty::Quant(QuantifiedType {
                    ident: right,
                    elements: right_elems,
                }),
            ) if left == right && left_elems.len() == right_elems.len() => {
                let mut subs = Substitution::new();
                for (l, r) in left_elems.into_iter().zip(right_elems.into_iter()) {
                    subs = subs.unify(l.unify(r, variance)?)?;
                }

                Ok(subs)
            }
            (left, right) => Err(UnificationError::TypesNotEqual(left, right)),
        }
    }

    #[cfg(test)]
    pub fn normalize(&self, subs: &Substitution<'src>) -> Self {
        match self {
            Ty::U32 => Ty::U32,
            Ty::Bool => Ty::Bool,
            Ty::Str => Ty::Str,
            Ty::Var(var) => match subs.get(var) {
                Some(t) => t.normalize(subs),
                None => Ty::Var(*var),
            },
            Ty::Quant(QuantifiedType { ident, elements }) => Ty::quantified(
                *ident,
                elements
                    .iter()
                    .map(|t| t.normalize(subs))
                    .collect::<Vec<_>>(),
            ),
        }
    }
}

impl<'src> From<TyVar> for Ty<'src> {
    fn from(value: TyVar) -> Self {
        Self::Var(value)
    }
}

impl<'src> From<&TyVar> for Ty<'src> {
    fn from(value: &TyVar) -> Self {
        Self::Var(*value)
    }
}

#[cfg(test)]
mod tests {
    use crate::types::{Substitution, Ty};

    #[test]
    fn normalize() {
        assert_eq!(Ty::U32.normalize(&Substitution::new()), Ty::U32);
        assert_eq!(Ty::Bool.normalize(&Substitution::new()), Ty::Bool);
        assert_eq!(Ty::Str.normalize(&Substitution::new()), Ty::Str);
        assert_eq!(
            Ty::var(0).normalize(&Substitution::from([
                (0.into(), Ty::var(1)),
                (1.into(), Ty::var(2)),
                (2.into(), Ty::var(3)),
            ])),
            Ty::var(3)
        );

        assert_eq!(
            Ty::quantified("Foo", [Ty::U32, Ty::var(0), Ty::var(4)]).normalize(
                &Substitution::from([
                    (0.into(), Ty::var(1)),
                    (1.into(), Ty::var(2)),
                    (2.into(), Ty::var(3)),
                ])
            ),
            Ty::quantified("Foo", [Ty::U32, Ty::var(3), Ty::var(4)])
        );
    }
}
