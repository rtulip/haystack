use super::{variance, Substitution, UnificationError, Variance};

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

    fn quantified<T>(ident: &'src str, ts: T) -> Self
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
}
