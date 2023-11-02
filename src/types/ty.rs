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

    pub fn fresh<'a>(&mut self) -> Ty<'a> {
        let t = Ty::var(self.0);
        self.0 += 1;
        t
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QuantifiedType<'a> {
    ident: &'a str,
    elements: Vec<Ty<'a>>,
}

impl<'a> QuantifiedType<'a> {
    fn new<const N: usize>(ident: &'a str, ts: [Ty<'a>; N]) -> Self {
        QuantifiedType {
            ident,
            elements: ts.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty<'a> {
    U32,
    Bool,
    Str,
    Var(TyVar),
    Quant(QuantifiedType<'a>),
}

impl<'a> Ty<'a> {
    fn var(n: usize) -> Self {
        Ty::Var(TyVar(n))
    }

    fn quantified<const N: usize>(ident: &'a str, ts: [Self; N]) -> Self {
        Self::Quant(QuantifiedType::new(ident, ts))
    }

    pub fn substitute(self, subs: &Substitution<'a>) -> Self {
        match self {
            Ty::U32 => Ty::U32,
            Ty::Bool => Ty::Bool,
            Ty::Str => Ty::Str,
            Ty::Var(var) => match subs.get(&var) {
                Some(ty) => ty.clone(),
                None => Ty::Var(var),
            },
            Ty::Quant(QuantifiedType { ident, elements }) => Ty::Quant(QuantifiedType {
                ident,
                elements: elements.into_iter().map(|t| t.substitute(subs)).collect(),
            }),
        }
    }

    pub fn unify(
        self,
        other: Self,
        variance: Variance,
    ) -> Result<Substitution<'a>, UnificationError<'a>> {
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
                todo!()
            }
            (left, right) => Err(UnificationError::TypesNotEqual(left, right)),
        }
    }
}
