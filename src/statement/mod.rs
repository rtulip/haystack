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

#[cfg(test)]
mod test_type_check {
    use crate::{
        expression::{AsExpr, BlockExpr, VarExpr},
        types::{Context, FnTy, QuantifiedType, Scheme, Substitution, Ty, TyGen},
    };

    use super::FunctionStmt;

    #[test]
    fn test_option_none() {
        let mut gen = TyGen::new();
        let option_t = Ty::Quant(QuantifiedType::new("Option", [gen.fresh()]));

        let ctx = Context::from([(
            "Option.None",
            Scheme::new([0.into()], FnTy::new([], [option_t])),
        )]);

        let x = FunctionStmt::new(
            VarExpr::from("Option.None").into(),
            Scheme::new(
                [],
                FnTy::new([], [Ty::Quant(QuantifiedType::new("Option", [Ty::U32]))]),
            ),
        )
        .type_check(&ctx, &mut gen);

        assert!(x.is_ok());
        let subs = x.unwrap();

        assert_eq!(subs, Substitution::from([(1.into(), Ty::U32)]));
    }

    #[test]
    fn dup() {
        let mut gen = TyGen::new();

        let ctx = Context::new();

        let free = gen.fresh();
        let x = FunctionStmt::new(
            BlockExpr::from([
                AsExpr::from(["x"]).into(),
                VarExpr::from("x").into(),
                VarExpr::from("x").into(),
            ])
            .into(),
            Scheme::new(
                [0.into()],
                FnTy::new([free.clone()], [free.clone(), free.clone()]),
            ),
        )
        .type_check(&ctx, &mut gen);

        assert!(x.is_ok());
        let subs = x.unwrap();
        assert_eq!(subs, Substitution::new());
    }
}
