mod fn_ty;
mod scheme;
mod sequence;
mod substitution;
mod ty;
mod variance;

pub use fn_ty::*;
pub use scheme::*;
pub use sequence::*;
pub use substitution::*;
pub use ty::*;
pub use variance::*;

use std::{convert::From, fmt::Debug};

#[derive(Debug)]
pub struct StackSplitError<'src>(Stack<'src>, usize);

#[derive(Debug)]
pub enum UnificationError<'src> {
    TypesNotEqual(Ty<'src>, Ty<'src>),
    StackLensDiffer(Stack<'src>, Stack<'src>),
}

pub struct Stack<'src>(Vec<Ty<'src>>);
impl<'src> Stack<'src> {
    fn split(mut self, len: usize) -> Result<(Stack<'src>, Stack<'src>), StackSplitError<'src>> {
        if len > self.0.len() {
            Err(StackSplitError(self, len))
        } else {
            let tail = self.0.split_off(self.0.len() - len);
            Ok((self, Stack(tail)))
        }
    }

    fn extend<T: IntoIterator<Item = Ty<'src>>>(&mut self, iter: T) {
        self.0.extend(iter)
    }

    fn unify(
        self,
        stack: Self,
        variance: Variance,
    ) -> Result<Substitution<'src>, UnificationError<'src>> {
        if self.0.len() != stack.0.len() {
            return Err(UnificationError::StackLensDiffer(self, stack));
        }

        let mut subs = Substitution::new();

        for (t1, t2) in self.into_iter().zip(stack.into_iter()) {
            let s = t1.unify(t2, variance)?;
            subs = subs.unify(s)?;
        }
        Ok(subs)
    }

    fn substitute(self, subs: &Substitution<'src>) -> Self {
        Self(self.0.into_iter().map(|t| t.substitute(subs)).collect())
    }
}

impl<'src, const N: usize> From<[Ty<'src>; N]> for Stack<'src> {
    fn from(value: [Ty<'src>; N]) -> Self {
        Self(value.into())
    }
}

impl<'src> IntoIterator for Stack<'src> {
    type Item = Ty<'src>;
    type IntoIter = std::vec::IntoIter<Ty<'src>>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'src> FromIterator<Ty<'src>> for Stack<'src> {
    fn from_iter<T: IntoIterator<Item = Ty<'src>>>(iter: T) -> Self {
        Self(Vec::from_iter(iter))
    }
}

impl<'src> Debug for Stack<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.0
                .iter()
                .map(|t| format!("{t:?}"))
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}

#[derive(Debug, Clone)]
pub struct Var<'src> {
    pub ident: &'src str,
    pub scheme: Scheme<'src>,
}

#[derive(Debug, Clone)]
pub struct Context<'src>(Vec<Var<'src>>);

impl<'src> Context<'src> {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn substitute(self, subs: &Substitution<'src>) -> Self {
        Self(
            self.0
                .into_iter()
                .map(|var| Var {
                    ident: var.ident,
                    scheme: var.scheme.substitute(subs),
                })
                .collect(),
        )
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Var<'src>> {
        self.0.iter()
    }
}

impl<'src, const N: usize> From<[(&'src str, Scheme<'src>); N]> for Context<'src> {
    fn from(value: [(&'src str, Scheme<'src>); N]) -> Self {
        Self(
            value
                .into_iter()
                .map(|(ident, scheme)| Var { ident, scheme })
                .collect(),
        )
    }
}
