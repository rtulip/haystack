use crate::types::{
    Context, FnTy, Scheme, Stack, StackSplitError, Substitution, TyGen, UnificationError, Var,
};

pub use self::{as_expr::AsExpr, block::BlockExpr, literal::LiteralExpr, var::VarExpr};
use std::convert::From;

mod as_expr;
mod block;
mod literal;
mod var;

#[derive(Debug, Clone)]
pub enum Expr<'src> {
    Literal(LiteralExpr<'src>),
    Block(BlockExpr<'src>),
    Var(VarExpr<'src>),
    As(AsExpr<'src>),
}

#[derive(Debug)]
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
    fn from(value: ()) -> Self {
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
                    Some(Var { scheme, .. }) => scheme.instantiate(gen),
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
