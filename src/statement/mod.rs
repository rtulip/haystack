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
        expression::{ApplicationError, AsExpr, BlockExpr, LiteralExpr, VarExpr},
        types::{
            Context, FnTy, QuantifiedType, Scheme, Stack, StackSplitError, Substitution, Ty, TyGen,
            UnificationError,
        },
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

    #[test]
    fn hello_world() {
        let mut gen = TyGen::new();

        let ctx = Context::from([
            ("putlns", Scheme::new([], FnTy::new([Ty::Str], []))),
            ("putlnu", Scheme::new([], FnTy::new([Ty::U32], []))),
            ("putlnb", Scheme::new([], FnTy::new([Ty::Bool], []))),
        ]);

        let main = FunctionStmt::new(
            BlockExpr::from([
                LiteralExpr::from(true).into(),
                LiteralExpr::from(12345).into(),
                LiteralExpr::from("Hello World").into(),
                VarExpr::from("putlns").into(),
                VarExpr::from("putlnu").into(),
                VarExpr::from("putlnb").into(),
            ])
            .into(),
            Scheme::new([], FnTy::new([], [])),
        )
        .type_check(&ctx, &mut gen);

        assert!(main.is_ok());
        let subs = main.unwrap();
        assert_eq!(subs, Substitution::new());
    }

    #[test]
    fn try_unify_diff_types() {
        let mut gen = TyGen::new();

        let ctx = Context::from([("putlns", Scheme::new([], FnTy::new([Ty::Str], [])))]);

        let main = FunctionStmt::new(
            BlockExpr::from([
                LiteralExpr::from(12345).into(),
                VarExpr::from("putlns").into(),
            ])
            .into(),
            Scheme::new([], FnTy::new([], [])),
        )
        .type_check(&ctx, &mut gen);

        assert_eq!(
            main,
            Err(ApplicationError::UnificationError(
                UnificationError::TypesNotEqual(Ty::U32, Ty::Str)
            ))
        );
    }

    #[test]
    fn too_few_arguments() {
        let mut gen = TyGen::new();

        let ctx = Context::from([("putlns", Scheme::new([], FnTy::new([Ty::Str], [])))]);

        let main = FunctionStmt::new(
            BlockExpr::from([VarExpr::from("putlns").into()]).into(),
            Scheme::new([], FnTy::new([], [])),
        )
        .type_check(&ctx, &mut gen);

        assert_eq!(
            main,
            Err(ApplicationError::TooFewElements(StackSplitError(
                Stack::from([]),
                1
            )))
        );
    }
}
