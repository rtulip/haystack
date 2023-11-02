use crate::{
    expression::{ApplicationError, Expr},
    types::{Context, Scheme, Stack, Substitution, TyGen, Variance},
};

pub struct FunctionStmt<'src> {
    expr: Expr<'src>,
    scheme: Scheme<'src>,
}

impl<'src> FunctionStmt<'src> {
    pub fn new(expr: Expr<'src>, scheme: Scheme<'src>) -> Self {
        Self { expr, scheme }
    }

    pub fn type_check(
        self,
        context: &Context<'src>,
        gen: &mut TyGen,
    ) -> Result<Substitution<'src>, ApplicationError<'src>> {
        let mut ctx = context.clone();
        let func = self.scheme.instantiate(gen);

        let (output, s1) = self
            .expr
            .apply(Stack::from_iter(func.input), &mut ctx, gen)?;
        let s2 = Stack::from_iter(func.output).unify(output, Variance::Contravariant)?;
        Ok(s1.unify(s2)?)
    }
}
