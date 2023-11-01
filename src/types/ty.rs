#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TyVar(usize);

impl From<usize> for TyVar {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

pub struct TyGen(usize);

impl TyGen {
    fn new() -> Self {
        Self(0)
    }
    fn fresh<'a>(&mut self) -> Ty<'a> {
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
}
