pub use self::{
    as_expr::AsExpr,
    block::BlockExpr,
    if_expr::IfExpr,
    literal::LiteralExpr,
    operator::{AddExpr, LessThanExpr, SubExpr},
    var::VarExpr,
};
use crate::{
    parser::token::Literal,
    types::{
        Context, FnTy, Scheme, Stack, StackSplitError, Substitution, Ty, TyGen, UnificationError,
        Var, Variance,
    },
};
use std::convert::From;

mod as_expr;
mod block;
mod if_expr;
mod literal;
mod operator;
mod var;

#[derive(Debug, Clone)]
pub enum Expr<'src> {
    Literal(LiteralExpr<'src>),
    Block(BlockExpr<'src>),
    Var(VarExpr<'src>),
    As(AsExpr<'src>),
    Add(AddExpr),
    Sub(SubExpr),
    LessThan(LessThanExpr),
    If(IfExpr<'src>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ApplicationError<'src> {
    TooFewElements(StackSplitError<'src>),
    UnificationError(UnificationError<'src>),
    UnknownVar(&'src str),
    Other,
}

impl<'src> From<StackSplitError<'src>> for ApplicationError<'src> {
    fn from(value: StackSplitError<'src>) -> Self {
        Self::TooFewElements(value)
    }
}

impl<'src> From<UnificationError<'src>> for ApplicationError<'src> {
    fn from(value: UnificationError<'src>) -> Self {
        Self::UnificationError(value)
    }
}

impl<'src> From<()> for ApplicationError<'src> {
    fn from(_: ()) -> Self {
        Self::Other
    }
}

impl<'src> Expr<'src> {
    pub fn apply<'ctx>(
        self,
        stack: Stack<'src>,
        context: &mut Context<'src>,
        gen: &mut TyGen,
    ) -> Result<(Stack<'src>, Substitution<'src>), ApplicationError<'src>> {
        let (stack, subs) = match self {
            Expr::Literal(lit) => {
                let func: FnTy<'_> = lit.into();
                func.apply(stack)?
            }
            Expr::Block(block) => block.apply(stack, context, gen)?,
            Expr::Var(VarExpr(var)) => {
                let func = match context.iter().rev().find(|Var { ident, .. }| *ident == var) {
                    Some(Var { scheme, .. }) => scheme.instantiate(gen).0,
                    None => return Err(ApplicationError::UnknownVar(var)),
                };
                func.apply(stack)?
            }
            Expr::As(AsExpr(bindings)) => {
                let (head, tail) = stack.split(bindings.len())?;
                for (ty, ident) in tail.into_iter().zip(bindings.into_iter()) {
                    context.push(ident, Scheme::new([], FnTy::new([], [ty])));
                }

                (head, Substitution::new())
            }
            Expr::Add(_) => FnTy::new([Ty::U32, Ty::U32], [Ty::U32]).apply(stack)?,
            Expr::Sub(_) => FnTy::new([Ty::U32, Ty::U32], [Ty::U32]).apply(stack)?,
            Expr::LessThan(_) => FnTy::new([Ty::U32, Ty::U32], [Ty::Bool]).apply(stack)?,
            Expr::If(IfExpr { then, otherwise }) => {
                let (stack, subs) = FnTy::new([Ty::Bool], []).apply(stack)?;

                let (then_stack, then_sub) = then.apply(stack.clone(), context, gen)?;
                let (otherwise_stack, otherwise_sub) =
                    otherwise.apply(stack.clone(), context, gen)?;

                let subs = subs.unify(then_sub)?.unify(otherwise_sub)?;

                let then_stack = then_stack.substitute(&subs);
                let otherwise_stack = otherwise_stack.substitute(&subs);

                let subs =
                    subs.unify(then_stack.unify(otherwise_stack.clone(), Variance::Covariant)?)?;
                (otherwise_stack.substitute(&subs), subs)
            }
        };

        Ok((stack, subs))
    }
}

impl<'src> From<BlockExpr<'src>> for Expr<'src> {
    fn from(value: BlockExpr<'src>) -> Self {
        Self::Block(value)
    }
}

impl<'src> From<LiteralExpr<'src>> for Expr<'src> {
    fn from(value: LiteralExpr<'src>) -> Self {
        Self::Literal(value)
    }
}

impl<'src> From<VarExpr<'src>> for Expr<'src> {
    fn from(value: VarExpr<'src>) -> Self {
        Self::Var(value)
    }
}

impl<'src> From<AsExpr<'src>> for Expr<'src> {
    fn from(value: AsExpr<'src>) -> Self {
        Self::As(value)
    }
}

impl<'src> From<AddExpr> for Expr<'src> {
    fn from(value: AddExpr) -> Self {
        Self::Add(value)
    }
}

impl<'src> From<LessThanExpr> for Expr<'src> {
    fn from(value: LessThanExpr) -> Self {
        Self::LessThan(value)
    }
}

impl<'src> From<SubExpr> for Expr<'src> {
    fn from(value: SubExpr) -> Self {
        Self::Sub(value)
    }
}

impl<'src> From<IfExpr<'src>> for Expr<'src> {
    fn from(value: IfExpr<'src>) -> Self {
        Self::If(value)
    }
}

impl<'src> From<Literal<'src>> for Expr<'src> {
    fn from(value: Literal<'src>) -> Self {
        match value {
            Literal::U32(n) => Expr::Literal(n.into()),
            Literal::Bool(b) => Expr::Literal(b.into()),
            Literal::String(s) => Expr::Literal(s.into()),
        }
    }
}
