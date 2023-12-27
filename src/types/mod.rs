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

use std::{collections::HashMap, convert::From, fmt::Debug};

use crate::{
    expression::Expr,
    interpreter::Element,
    statement::{FunctionStmt, ImplStmt},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackSplitError<'src>(pub Stack<'src>, pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnificationError<'src> {
    TypesNotEqual(Ty<'src>, Ty<'src>),
    StackLensDiffer(Stack<'src>, Stack<'src>),
}

pub type Types<'src> = HashMap<&'src str, Ty<'src>>;

#[derive(Clone, PartialEq, Eq)]
pub struct Stack<'src>(Vec<Ty<'src>>);
impl<'src> Stack<'src> {
    pub fn split(
        mut self,
        len: usize,
    ) -> Result<(Stack<'src>, Stack<'src>), StackSplitError<'src>> {
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

    pub fn unify(
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

    pub fn substitute(self, subs: &Substitution<'src>) -> Self {
        Self(self.0.into_iter().map(|t| t.substitute(subs)).collect())
    }

    pub fn len(&self) -> usize {
        self.0.len()
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
    pub ident: String,
    pub scheme: Scheme<'src>,
}

#[derive(Debug, Clone)]
pub struct Context<'src>(Vec<Var<'src>>);

impl<'src> Context<'src> {
    pub fn new() -> Self {
        let mut ctx = Context(vec![]);
        ctx.push(
            "__builtin_print_bool",
            Scheme::new([], FnTy::new([Ty::Bool], [])),
        );
        ctx.push(
            "__builtin_print_u32",
            Scheme::new([], FnTy::new([Ty::U32], [])),
        );
        ctx.push(
            "__builtin_print_string",
            Scheme::new([], FnTy::new([Ty::Str], [])),
        );

        ctx
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Var<'src>> {
        self.0.iter()
    }

    pub fn push<S>(&mut self, ident: S, scheme: Scheme<'src>)
    where
        S: Into<String>,
    {
        self.0.push(Var {
            ident: ident.into(),
            scheme,
        })
    }
}

// impl<'src, const N: usize> From<[(&'src str, Scheme<'src>, Element<'src>); N]> for Context<'src> {
//     fn from(value: [(&'src str, Scheme<'src>, Element<'src>); N]) -> Self {
//         Self(
//             value
//                 .into_iter()
//                 .map(|(ident, scheme, element)| Var {
//                     ident: ident.into(),
//                     scheme,
//                     element,
//                 })
//                 .collect(),
//         )
//     }
// }

#[cfg(test)]
mod test {
    use crate::types::{Ty, UnificationError};

    use super::{Stack, Variance};

    #[test]
    fn unify_diff_len_stacks() {
        assert_eq!(
            Stack::from([Ty::U32]).unify(Stack::from([]), Variance::Covariant),
            Err(UnificationError::StackLensDiffer(
                Stack::from([Ty::U32]),
                Stack::from([])
            ))
        )
    }

    #[test]
    fn format_stack() {
        assert_eq!(
            format!("{:?}", Stack::from([Ty::U32, Ty::Str])),
            format!("[U32 Str]")
        )
    }
}
