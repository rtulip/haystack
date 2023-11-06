use crate::types::{Context, Stack, Substitution, TyGen};

use super::{ApplicationError, Expr};
use std::convert::From;

#[derive(Debug, Clone)]
pub struct BlockExpr<'src>(Vec<Expr<'src>>);

impl<'src> BlockExpr<'src> {
    pub fn apply<'ctx>(
        self,
        stack: Stack<'src>,
        context: &mut Context<'src>,
        gen: &mut TyGen,
    ) -> Result<(Stack<'src>, Substitution<'src>), ApplicationError<'src>> {
        let mut ctx = context.clone();
        let mut stack_outer = stack;
        let mut subs_outer = Substitution::new();
        for e in self.0 {
            let (stack, sub) = e.apply(stack_outer, &mut ctx, gen)?;
            subs_outer = subs_outer.unify(sub)?;
            stack_outer = stack
                .into_iter()
                .map(|t| t.substitute(&subs_outer))
                .collect();
        }

        *context = ctx;

        Ok((stack_outer, subs_outer))
    }
}

impl<'src, T> From<T> for BlockExpr<'src>
where
    T: Into<Vec<Expr<'src>>>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}

impl<'src> IntoIterator for BlockExpr<'src> {
    type Item = Expr<'src>;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
