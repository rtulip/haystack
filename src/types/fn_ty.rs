use std::convert::From;
use std::fmt::Debug;

use crate::{
    expression::{ApplicationError, LiteralExpr},
    parser::token::Token,
};

use super::{sequence::TySeq, ty::Ty, Stack, Substitution, Variance};

#[derive(Clone)]
pub struct FnTy<'a> {
    pub input: TySeq<'a>,
    pub output: TySeq<'a>,
}

impl<'a> FnTy<'a> {
    pub fn new<IN, OUT>(input: IN, output: OUT) -> Self
    where
        IN: Into<TySeq<'a>>,
        OUT: Into<TySeq<'a>>,
    {
        Self {
            input: input.into(),
            output: output.into(),
        }
    }

    pub fn apply(
        self,
        token: &Token<'a>,
        stack: Stack<'a>,
    ) -> Result<(Stack<'a>, Substitution<'a>), ApplicationError<'a>> {
        let (mut head, tail) = stack
            .split(self.input.len())
            .map_err(|e| ApplicationError::TooFewElements(token.clone(), e))?;

        let subs = tail
            .unify(Stack(self.input), Variance::Covariant)
            .map_err(|e| ApplicationError::UnificationError(token.clone(), e))?;
        head.extend(self.output);

        Ok((head.substitute(&subs), subs))
    }

    pub fn substitute(self, subs: &Substitution<'a>) -> Self {
        Self {
            input: self.input.into_iter().map(|t| t.substitute(subs)).collect(),
            output: self
                .output
                .into_iter()
                .map(|t| t.substitute(subs))
                .collect(),
        }
    }

    pub fn apply_with_predicate<P>(
        self,
        token: &Token<'a>,
        stack: Stack<'a>,
        predicate: P,
    ) -> Result<(Stack<'a>, Substitution<'a>), ApplicationError<'a>>
    where
        P: Fn(&Substitution<'a>) -> Result<(), ApplicationError<'a>>,
    {
        let (stk, subs) = self.apply(token, stack)?;
        predicate(&subs)?;
        Ok((stk, subs))
    }
}

impl<'a> From<LiteralExpr<'a>> for FnTy<'a> {
    fn from(value: LiteralExpr<'a>) -> Self {
        match value {
            LiteralExpr::U32(_) => FnTy::new([], [Ty::U32]),
            LiteralExpr::String(_) => FnTy::new([], [Ty::Str]),
            LiteralExpr::Bool(_) => FnTy::new([], [Ty::Bool]),
        }
    }
}

impl<'a> Debug for FnTy<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}) -> [{}]",
            self.input
                .iter()
                .map(|t| format!("{t:?}"))
                .collect::<Vec<_>>()
                .join(" "),
            self.output
                .iter()
                .map(|t| format!("{t:?}"))
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::types::{FnTy, Ty};

    #[test]
    fn format_fn() {
        assert_eq!(
            format!("{:?}", FnTy::new([Ty::U32, Ty::Bool], [Ty::Str])),
            format!("(U32 Bool) -> [Str]")
        )
    }
}
