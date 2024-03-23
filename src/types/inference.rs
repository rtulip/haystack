use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::{
    expr::{BinOp, Expr, Literal},
    union_find::{UnifiactionError, UnificationTable},
};

use super::{constraint::Constraint, scheme::Scheme, Stack, Type, TypeVar, Var};

pub struct ConstrainedType {
    var_ty: Option<Type>,
    constriants: Vec<Constraint>,
}

impl ConstrainedType {
    fn new<C>(ty: Option<Type>, constraints: C) -> Self
    where
        C: Into<Vec<Constraint>>,
    {
        Self {
            var_ty: ty,
            constriants: constraints.into(),
        }
    }
}

pub struct TypeInference {
    lookup: UnificationTable<TypeVar, Type>,
}

#[derive(Debug, Clone, Copy)]
pub enum Typing {
    Exact,
    Subtype,
}

enum InferenceError {
    StackUnderflow,
    ExtensionExpressionReached,
}

enum TypeUnificationError {
    NotEqual(Type, Type),
}

impl TypeInference {
    pub fn new() -> Self {
        Self {
            lookup: UnificationTable::new(),
        }
    }

    fn fresh_type_var(&mut self) -> TypeVar {
        self.lookup.new_key()
    }

    fn freshen(&mut self, ty: Type, fresh: &mut HashMap<TypeVar, Type>) -> Type {
        match ty {
            Type::Var(var) => {
                if let Some(ty) = fresh.get(&var) {
                    ty.clone()
                } else {
                    let ty = Type::Var(self.fresh_type_var());
                    fresh.insert(var, ty.clone());
                    ty
                }
            }
            Type::U32 => Type::U32,
            Type::U8 => Type::U8,
            Type::Bool => Type::Bool,
            Type::Pointer { ty, mutable } => Type::Pointer {
                ty: Box::new(self.freshen(*ty, fresh)),
                mutable,
            },
            Type::Func { input, output } => Type::Func {
                input: self.freshen_many(input, fresh),
                output: self.freshen_many(output, fresh),
            },
            Type::String => Type::String,
            Type::Never => Type::Never,
        }
    }

    fn freshen_many(&mut self, tys: Vec<Type>, fresh: &mut HashMap<TypeVar, Type>) -> Vec<Type> {
        tys.into_iter().map(|ty| self.freshen(ty, fresh)).collect()
    }

    fn infer<'a, M>(
        &mut self,
        stack: &mut Stack,
        env: &mut HashMap<Var, Type>,
        expr: Expr<'a, M, ()>,
    ) -> Result<Expr<'a, (M, ConstrainedType), ()>, InferenceError> {
        match expr.expr {
            crate::expr::ExprBase::Literal(Literal::Bool(b)) => {
                stack.push(Type::Bool);
                Ok(Expr::literal(
                    Literal::Bool(b),
                    (expr.meta, ConstrainedType::new(None, [])),
                ))
            }
            crate::expr::ExprBase::Literal(Literal::U32(n)) => {
                stack.push(Type::U32);
                Ok(Expr::literal(
                    Literal::U32(n),
                    (expr.meta, ConstrainedType::new(None, [])),
                ))
            }
            crate::expr::ExprBase::Literal(Literal::U8(n)) => {
                stack.push(Type::U8);
                Ok(Expr::literal(
                    Literal::U8(n),
                    (expr.meta, ConstrainedType::new(None, [])),
                ))
            }
            crate::expr::ExprBase::Literal(Literal::String(s)) => {
                stack.push(Type::String);
                Ok(Expr::literal(
                    Literal::String(s),
                    (expr.meta, ConstrainedType::new(None, [])),
                ))
            }
            crate::expr::ExprBase::Print => {
                let t = stack.pop().ok_or_else(|| InferenceError::StackUnderflow)?;
                Ok(Expr::print((
                    expr.meta,
                    ConstrainedType::new(None, [Constraint::Equal(t, Type::U32)]),
                )))
            }
            crate::expr::ExprBase::PrintString => {
                let t = stack.pop().ok_or_else(|| InferenceError::StackUnderflow)?;
                Ok(Expr::print_string((
                    expr.meta,
                    ConstrainedType::new(None, [Constraint::Equal(t, Type::String)]),
                )))
            }
            crate::expr::ExprBase::Block(exprs) => {
                let mut local_env = env.clone();

                let exprs = exprs
                    .into_iter()
                    .map(|e| self.infer(stack, &mut local_env, e))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(Expr::block(
                    exprs,
                    (expr.meta, ConstrainedType::new(None, [])),
                ))
            }
            crate::expr::ExprBase::BinOp(op) => match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                    let right = stack.pop().ok_or_else(|| InferenceError::StackUnderflow)?;
                    let left = stack.pop().ok_or_else(|| InferenceError::StackUnderflow)?;
                    stack.push(Type::U32);
                    Ok(Expr::binop(
                        op,
                        (
                            expr.meta,
                            ConstrainedType::new(
                                None,
                                [
                                    Constraint::Equal(right, Type::U32),
                                    Constraint::Equal(left, Type::U32),
                                ],
                            ),
                        ),
                    ))
                }
                _ => todo!(),
            },
            crate::expr::ExprBase::Call(f) => {
                let ty = env.get(&f).expect("Vars should be resolvable").clone();
                let (input, output) = self.freshen(ty, &mut HashMap::new()).expect_function();

                if stack.len() < input.len() {
                    todo!("not enough elements on the stack to call function");
                }

                let split = stack.split_off(stack.len() - input.len());
                let constraints = split
                    .into_iter()
                    .zip(input.clone().into_iter())
                    .map(|(left, right)| Constraint::Subtype {
                        parent: right,
                        child: left,
                    })
                    .collect::<Vec<_>>();
                stack.extend(output.clone());

                Ok(Expr::call(
                    f,
                    (
                        expr.meta,
                        ConstrainedType::new(Some(Type::Func { input, output }), constraints),
                    ),
                ))
            }
            crate::expr::ExprBase::Ext(_) => Err(InferenceError::ExtensionExpressionReached),
        }
    }

    fn unification(&mut self, constraints: Vec<Constraint>) -> Result<(), TypeUnificationError> {
        for c in constraints {
            match c {
                Constraint::Equal(left, right) => self.unify_ty_ty(left, right, Typing::Exact)?,
                Constraint::Subtype { parent, child } => {
                    self.unify_ty_ty(parent, child, Typing::Subtype)?
                }
            }
        }

        Ok(())
    }

    fn normalize_ty(&mut self, ty: Type) -> Type {
        match ty {
            Type::U32 => Type::U32,
            Type::U8 => Type::U8,
            Type::Bool => Type::Bool,
            Type::Var(var) => match self.lookup.get(var) {
                Some(ty) => self.normalize_ty(ty.clone()),
                None => Type::Var(var),
            },
            Type::Pointer { ty, mutable } => Type::Pointer {
                ty: Box::new(self.normalize_ty(*ty)),
                mutable,
            },
            Type::Func { input, output } => Type::Func {
                input: input.into_iter().map(|ty| self.normalize_ty(ty)).collect(),
                output: output.into_iter().map(|ty| self.normalize_ty(ty)).collect(),
            },
            Type::String => Type::String,
            Type::Never => Type::Never,
        }
    }

    fn unify_ty_ty(
        &mut self,
        unnorm_left: Type,
        unnorm_right: Type,
        typing: Typing,
    ) -> Result<(), TypeUnificationError> {
        let left = self.normalize_ty(unnorm_left);
        let right = self.normalize_ty(unnorm_right);

        match (left, right) {
            (Type::U32, Type::U32) => Ok(()),
            (Type::U8, Type::U8) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::Var(left), Type::Var(right)) => {
                self.lookup
                    .unify_keys(left, right)
                    .map_err(|UnifiactionError(left, right)| {
                        TypeUnificationError::NotEqual(left, right)
                    })
            }
            (Type::Var(var), ty) | (ty, Type::Var(var)) => self
                .lookup
                .assign_key(var, Some(ty))
                .map_err(|UnifiactionError(left, right)| {
                    TypeUnificationError::NotEqual(left, right)
                }),
            (
                Type::Pointer {
                    ty: left,
                    mutable: left_mut,
                },
                Type::Pointer {
                    ty: right,
                    mutable: right_mut,
                },
            ) => todo!(),
            (Type::String, Type::String) => Ok(()),
            (Type::Never, Type::Never) => Ok(()),
            (Type::Never, ty) | (ty, Type::Never) => {
                Err(TypeUnificationError::NotEqual(Type::Never, ty))
            }
            (left, right) => Err(TypeUnificationError::NotEqual(left, right)),
        }
    }

    fn substitute_one(&mut self, ty: Type) -> (HashSet<TypeVar>, Type) {
        match ty {
            Type::Never | Type::String | Type::U32 | Type::U8 | Type::Bool => (HashSet::new(), ty),
            Type::Var(var) => match self.lookup.get(var) {
                Some(ty) => self.substitute_one(ty.clone()),
                None => (HashSet::from([var]), Type::Var(var)),
            },
            Type::Pointer { ty, mutable } => {
                let (subs, ty) = self.substitute_one(*ty);
                (
                    subs,
                    Type::Pointer {
                        ty: Box::new(ty),
                        mutable,
                    },
                )
            }
            Type::Func { input, output } => {
                let (in_subs, input) = self.substitute(input);
                let (out_subs, output) = self.substitute(output);

                (
                    in_subs.into_iter().chain(out_subs.into_iter()).collect(),
                    Type::Func { input, output },
                )
            }
        }
    }

    fn substitute(&mut self, stack: Stack) -> (HashSet<TypeVar>, Vec<Type>) {
        let mut unbound = HashSet::new();
        let mut stk = vec![];

        for ty in stack {
            let (ub, ty) = self.substitute_one(ty);
            unbound.extend(ub);
            stk.push(ty);
        }

        (unbound, stk)
    }

    fn substitute_expr<'a, M>(
        &mut self,
        expr: Expr<'a, (M, ConstrainedType), ()>,
    ) -> (HashSet<TypeVar>, Expr<'a, (M, ConstrainedType), ()>) {
        match expr.expr {
            crate::expr::ExprBase::Literal(_) => (HashSet::new(), expr),
            crate::expr::ExprBase::Print => (HashSet::new(), expr),
            crate::expr::ExprBase::PrintString => (HashSet::new(), expr),
            crate::expr::ExprBase::Block(exprs) => {
                let mut unbound = HashSet::new();
                let exprs = exprs
                    .into_iter()
                    .map(|e| self.substitute_expr(e))
                    .map(|(ub, e)| {
                        unbound.extend(ub);
                        e
                    })
                    .collect::<Vec<_>>();

                (unbound, Expr::block(exprs, expr.meta))
            }
            crate::expr::ExprBase::BinOp(_) => (HashSet::new(), expr),
            crate::expr::ExprBase::Call(Var::Func(f)) => {
                let (input, output) = expr
                    .meta
                    .1
                    .var_ty
                    .expect("Call exprs should have `var_ty`, but found none ")
                    .clone()
                    .expect_function();

                let (mut unbound, sub_input) = self.substitute(input);
                let (out_unbound, sub_output) = self.substitute(output);

                unbound.extend(out_unbound);

                let fn_ty = Type::Func {
                    input: sub_input,
                    output: sub_output,
                };

                (
                    unbound,
                    Expr::call(
                        Var::Func(f),
                        (
                            expr.meta.0,
                            ConstrainedType::new(Some(fn_ty), expr.meta.1.constriants),
                        ),
                    ),
                )
            }
            crate::expr::ExprBase::Call(Var::Ident(_)) => unreachable!("Can't call an ident"),
            crate::expr::ExprBase::Ext(_) => unreachable!(),
        }
    }

    pub fn type_check<'a, M>(
        &mut self,
        expr: Expr<'a, M, ()>,
        parent_env: &HashMap<Var, Type>,
        stack: &mut Stack,
        target: Stack,
    ) -> Result<(Expr<'a, (M, ConstrainedType), ()>, Scheme), TypeCheckError> {
        let (mut unbound, _) = self.substitute(stack.clone());
        let mut env = parent_env.clone();
        let mut out = self.infer(stack, &mut env, expr).map_err(|e| match e {
            InferenceError::StackUnderflow => TypeCheckError::StackUnderflow,
            InferenceError::ExtensionExpressionReached => {
                TypeCheckError::ExtensionExpressionReached
            }
        })?;

        out.meta
            .1
            .constriants
            .extend(Constraint::stack_compare(target, stack.clone()));

        // TODO: Constrain Type Vars here.

        self.unification(out.meta.1.constriants.clone())
            .map_err(|e| match e {
                TypeUnificationError::NotEqual(a, b) => TypeCheckError::TypesNotEqual(a, b),
            })?;

        let (unbound_stk, stk) = self.substitute(stack.clone());
        let (unbound_expr, expr) = self.substitute_expr(out);

        unbound.extend(unbound_stk);
        unbound.extend(unbound_expr);

        Ok((
            expr,
            Scheme {
                unbound,
                stack: stk,
            },
        ))
    }
}

#[derive(Debug)]
pub enum TypeCheckError {
    StackUnderflow,
    ExtensionExpressionReached,
    TypesNotEqual(Type, Type),
}
