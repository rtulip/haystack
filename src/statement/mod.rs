use std::collections::HashMap;

use crate::{
    expression::{ApplicationError, Expr},
    parser::token::Token,
    types::{Context, Scheme, Stack, Substitution, Ty, TyGen, Variance},
};

pub struct FunctionStmt<'src> {
    pub token: Token<'src>,
    pub expr: Expr<'src>,
    scheme: Scheme<'src>,
}

pub struct TypeDefStmt<'src> {
    pub name: Token<'src>,
    pub ty: Ty<'src>,
}

pub enum Stmt<'src> {
    Function(FunctionStmt<'src>),
    TypeDef(TypeDefStmt<'src>),
}

impl<'src> From<FunctionStmt<'src>> for Stmt<'src> {
    fn from(value: FunctionStmt<'src>) -> Self {
        Stmt::Function(value)
    }
}

impl<'src> FunctionStmt<'src> {
    pub fn new(token: Token<'src>, expr: Expr<'src>, scheme: Scheme<'src>) -> Self {
        Self {
            token,
            expr,
            scheme,
        }
    }

    pub fn type_check(
        &self,
        types: &Types<'src>,
        context: &Context<'src>,
        gen: &mut TyGen,
    ) -> Result<Substitution<'src>, ApplicationError<'src>> {
        let mut ctx = context.clone();
        let (func, subs) = self.scheme.concrete_instantiation(gen);
        let (output, s1) =
            self.expr
                .clone()
                .apply(Stack::from_iter(func.input), types, &mut ctx, gen)?;
        let s2 = Stack::from_iter(func.output)
            .unify(output, Variance::Contravariant)
            .map_err(|e| ApplicationError::UnificationError(self.token.clone(), e))?;

        let subs = subs
            .unify(
                s1.unify(s2)
                    .map_err(|e| ApplicationError::UnificationError(self.token.clone(), e))?,
            )
            .map_err(|e| ApplicationError::UnificationError(self.token.clone(), e))?;

        Ok(subs)
    }

    pub fn token(&self) -> &Token<'src> {
        &self.token
    }

    pub fn scheme(&self) -> &Scheme<'src> {
        &self.scheme
    }

    pub fn resolve_names(&mut self, types: &Types<'src>) -> Result<(), ()> {
        self.expr.resolve_names(types)
    }
}

#[cfg(test)]
mod test_type_check {
    use crate::{
        expression::{
            AddExpr, ApplicationError, AsExpr, BlockExpr, Expr, IfExpr, LessThanExpr, LiteralExpr,
            SubExpr, VarExpr,
        },
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

        let x = FunctionStmt::new_wo_token(
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
        let x = FunctionStmt::new_wo_token(
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
        assert_eq!(subs, Substitution::from([(0.into(), Ty::var(1))]));
    }

    #[test]
    fn hello_world() {
        let mut gen = TyGen::new();

        let ctx = Context::from([
            ("putlns", Scheme::new([], FnTy::new([Ty::Str], []))),
            ("putlnu", Scheme::new([], FnTy::new([Ty::U32], []))),
            ("putlnb", Scheme::new([], FnTy::new([Ty::Bool], []))),
        ]);

        let main = FunctionStmt::new_wo_token(
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

        let main = FunctionStmt::new_wo_token(
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

        let main = FunctionStmt::new_wo_token(
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

    #[test]
    fn context_inference() {
        let mut gen = TyGen::new();

        let ctx = Context::from([
            ("putlns", Scheme::new([], FnTy::new([Ty::Str], []))),
            ("putlnu", Scheme::new([], FnTy::new([Ty::U32], []))),
            ("putlnb", Scheme::new([], FnTy::new([Ty::Bool], []))),
        ]);
        let (ty, var) = gen.fresh_with_var();
        let foo = FunctionStmt::new_wo_token(
            BlockExpr::from([
                AsExpr::from(["x"]).into(),
                VarExpr::from("x").into(),
                VarExpr::from("x").into(),
                AddExpr.into(),
                VarExpr::from("putlnu").into(),
                VarExpr::from("x").into(),
                VarExpr::from("putlns").into(),
            ])
            .into(),
            Scheme::new([var], FnTy::new([ty], [])),
        )
        .type_check(&ctx, &mut gen);

        assert_eq!(
            foo,
            Err(ApplicationError::UnificationError(
                UnificationError::TypesNotEqual(Ty::U32, Ty::Str)
            ))
        );

        let (ty, var) = gen.fresh_with_var();
        let foo = FunctionStmt::new_wo_token(
            BlockExpr::from([
                AsExpr::from(["x"]).into(),
                VarExpr::from("x").into(),
                VarExpr::from("x").into(),
                AddExpr.into(),
                VarExpr::from("putlnu").into(),
            ])
            .into(),
            Scheme::new([var], FnTy::new([ty], [])),
        )
        .type_check(&ctx, &mut gen);

        assert!(foo.is_ok());
        assert_eq!(Ty::from(var).normalize(&foo.unwrap()), Ty::U32)
    }

    #[test]
    fn fib() {
        let mut gen = TyGen::new();

        let ctx = Context::from([("fib", Scheme::new([], FnTy::new([Ty::U32], [Ty::U32])))]);
        let fib = FunctionStmt::new_wo_token(
            BlockExpr::from([
                AsExpr::from("n").into(),
                VarExpr::from("n").into(),
                LiteralExpr::from(2).into(),
                LessThanExpr.into(),
                IfExpr::new(
                    VarExpr::from("n"),
                    BlockExpr::from([
                        VarExpr::from("n").into(),
                        LiteralExpr::from(1).into(),
                        SubExpr.into(),
                        VarExpr::from("fib").into(),
                        VarExpr::from("n").into(),
                        LiteralExpr::from(2).into(),
                        SubExpr.into(),
                        VarExpr::from("fib").into(),
                        AddExpr.into(),
                    ]),
                )
                .into(),
            ])
            .into(),
            Scheme::new([], FnTy::new([Ty::U32], [Ty::U32])),
        );

        assert!(fib.type_check(&ctx, &mut gen).is_ok());
    }

    #[test]
    fn hard_if_expr() {
        let mut gen = TyGen::new();

        let (ty, var) = gen.fresh_with_var();
        let mut ctx = Context::from([
            (
                "Option.None",
                Scheme::new([var], FnTy::new([], [ty.clone()])),
            ),
            (
                "Option.Some",
                Scheme::new(
                    [var],
                    FnTy::new([Ty::Var(var)], [Ty::quantified("Option", vec![ty.clone()])]),
                ),
            ),
            ("drop", Scheme::new([var], FnTy::new([ty.clone()], []))),
            (
                "Option.Unwrap<u32>",
                Scheme::new(
                    [],
                    FnTy::new([Ty::quantified("Option", [Ty::U32])], [Ty::U32]),
                ),
            ),
        ]);

        let e: Expr<'_> = BlockExpr::from([
            IfExpr::new(
                VarExpr::from("Option.Some"),
                BlockExpr::from([
                    VarExpr::from("drop").into(),
                    VarExpr::from("Option.None").into(),
                ]),
            )
            .into(),
            VarExpr::from("Option.Unwrap<u32>").into(),
        ])
        .into();

        let t1 = gen.fresh();
        let t2 = gen.fresh();

        let (stack, subs) = e
            .apply(
                Stack::from([Ty::U32, t1.clone(), t2.clone(), t1.clone()]),
                &mut ctx,
                &mut gen,
            )
            .unwrap();

        assert_eq!(stack, Stack::from([Ty::U32, Ty::Bool, Ty::U32]));
        assert_eq!(t1.normalize(&subs), Ty::Bool);
        assert_eq!(t2.normalize(&subs), Ty::U32);
    }
}
