use std::{fmt::Debug, todo, vec};

use crate::typecheck::{environment::{Coherence, Environment, InterfaceImpl, TypePredicate}, expr::Expr, typ::{typ::FunctionType, type_id::TypeId, type_var::TypeVar}, typestore::{TypeStore, inference::InferenceError}};

use super::constraint::ConstraintOrigin;

#[derive(Debug, PartialEq, Eq)]
pub struct Function<'src, Meta> {
    pub ty: FunctionType,
    pub params: Vec<TypeVar>,
    pub predicates: Vec<TypePredicate>,
    pub expr: Expr<'src, Meta>,
}

impl<'src, Meta> Function<'src, Meta>
where
    Meta: std::fmt::Debug + Clone,
{
    pub fn new<Input, Output, Preds, Vars>(
        input: Input,
        output: Output,
        params: Vars,
        predicates: Preds,
        expr: Expr<'src, Meta>,
    ) -> Self
    where
        Input: Into<Vec<TypeId>>,
        Output: Into<Vec<TypeId>>,
        Vars: Into<Vec<TypeVar>>,
        Preds: Into<Vec<TypePredicate>>,
    {
        Self {
            ty: FunctionType {
                input: input.into(),
                output: output.into(),
            },
            params: params.into(),
            predicates: predicates.into(),
            expr,
        }
    }

    pub fn type_check(
        self,
        typestore: &mut TypeStore<Meta>,
        env: &Environment,
    ) -> Result<Function<'src, (Meta, TypeId)>, InferenceError<Meta>>
    where
        Meta: Debug + Clone + PartialEq + Eq,
    {
        typestore.solidify_vars(&self.params)?;

        let mut env = env.clone();

        for p in &self.predicates {
            let (unbound, p) = typestore.substitute_predicate(p);
            if !unbound.is_empty() {
                todo!("I'd expect that predicates are concrete for typechecking functions");
            }

            let p_impl = InterfaceImpl {
                cid: p.cid,
                vars: vec![],
                args: p.args,
                predicates: vec![],
                coherence: Coherence::Overlapping,
            };

            env.add_impl(typestore, p.cid, p_impl, &self.expr.meta)?;
        }

        let (expr, unbound) =
            typestore.type_check(self.expr, &self.ty.input, &self.ty.output, &mut env)?;

        if !unbound.is_empty() {
            let mut vars: Vec<_> = unbound.into_iter().collect();
            vars.sort();

            return Err(InferenceError::unbound_var(
                vars[0],
                &ConstraintOrigin::UnboundVarInBody,
                &expr.meta.0,
            ));
        }

        Ok(Function {
            ty: self.ty,
            params: self.params,
            predicates: self.predicates,
            expr,
        })
    }
}
