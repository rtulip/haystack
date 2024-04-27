use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use crate::{
    c::CType,
    expr::{BinOp, Expr, Literal},
    types::FunctionType,
    union_find::{UnifiactionError, UnificationTable},
};

use super::{constraint::Constraint, scheme::Scheme, Stack, StructType, Type, TypeVar, Var};

#[derive(Debug, Clone)]
pub struct ConstrainedType {
    pub var_ty: Option<Type>,
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

    pub fn is_free(&self) -> bool {
        match &self.var_ty {
            Some(ty) => ty.is_free(),
            None => todo!(),
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
    DifferentStackLengths(Stack, Stack),
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

    pub fn new_from<'a, Ts>(ts: Ts) -> Self
    where
        Ts: IntoIterator<Item = &'a Type>,
    {
        let mut free_vars = ts
            .into_iter()
            .map(|ty| ty.free_vars())
            .flatten()
            .collect::<Vec<_>>();
        free_vars.sort();

        let mut inference = Self::new();
        match free_vars.last() {
            Some(TypeVar(n)) => {
                // Make sure that type vars up to and includeing the largest
                // type variable (if present) is known.
                for _ in 0..=*n {
                    inference.fresh_type_var();
                }
            }
            None => (),
        }

        inference
    }

    fn env_get_num_idents(env: &HashMap<Var, Type>) -> usize {
        env.iter().fold(0, |acc, (var, _)| {
            if matches!(var, Var::Ident(_)) {
                acc + 1
            } else {
                acc
            }
        })
    }

    pub fn fresh_type_var(&mut self) -> TypeVar {
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
            Type::Func(f) => {
                let params = self.freshen_many(f.params.clone(), fresh);

                let tbl = f.params.iter().zip(params.iter()).collect::<Vec<_>>();
                let input = f
                    .input
                    .into_iter()
                    .map(|t| match tbl.iter().find(|(prev, _)| &t == *prev) {
                        Some((Type::Var(_), fresh_ty)) => (*fresh_ty).clone(),
                        Some(_) => t,
                        None => t,
                    })
                    .collect();

                let output = f
                    .output
                    .into_iter()
                    .map(|t| match tbl.iter().find(|(prev, _)| &t == *prev) {
                        Some((Type::Var(_), fresh_ty)) => (*fresh_ty).clone(),
                        Some(_) => t,
                        None => t,
                    })
                    .collect();

                Type::Func(FunctionType {
                    input,
                    output,
                    params,
                })
            }
            Type::Struct(s) => Type::Struct(StructType {
                name: s.name,
                members: s
                    .members
                    .into_iter()
                    .map(|(id, ty)| (id, self.freshen(ty, fresh)))
                    .collect(),
            }),
            Type::TaggedUnion(tu) => todo!(),
            Type::Tuple(_) => todo!(),
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
        extern_fns: &HashMap<usize, CType>,
        expr: Expr<'a, M, (), Type>,
    ) -> Result<Expr<'a, (M, ConstrainedType), (), Type>, InferenceError>
    where
        M: Debug,
    {
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
                stack.push(Type::string());
                Ok(Expr::literal(
                    Literal::String(s),
                    (expr.meta, ConstrainedType::new(None, [])),
                ))
            }
            crate::expr::ExprBase::Block(exprs) => {
                let mut local_env = env.clone();

                let exprs = exprs
                    .into_iter()
                    .map(|e| self.infer(stack, &mut local_env, extern_fns, e))
                    .collect::<Result<Vec<_>, _>>()?;

                let mut constraints = vec![];
                exprs.iter().for_each(|e| {
                    e.for_each_meta(|(_, constrained_ty)| {
                        constraints.extend(constrained_ty.constriants.clone())
                    })
                });

                Ok(Expr::block(
                    exprs,
                    (expr.meta, ConstrainedType::new(None, constraints)),
                ))
            }
            crate::expr::ExprBase::While { condition, body } => {
                let stack_before = stack.clone();

                let mut condition_env = env.clone();
                let condition = condition
                    .into_iter()
                    .map(|e| self.infer(stack, &mut condition_env, extern_fns, e))
                    .collect::<Result<Vec<_>, _>>()?;

                // Pop a boolean off the top of the stack to see if body should be run.
                let cond_t = stack.pop().ok_or_else(|| InferenceError::StackUnderflow)?;

                let body = self.infer(stack, env, extern_fns, *body)?;

                let mut constraints = vec![Constraint::Equal(cond_t, Type::Bool)];
                condition.iter().for_each(|e| {
                    e.for_each_meta(|(_, constrained_ty)| {
                        constraints.extend(constrained_ty.constriants.clone())
                    })
                });

                constraints.extend(body.meta.1.constriants.clone());

                // Make sure that the stack lengths didn't change from before the while loop and after.
                if stack_before.len() != stack.len() {
                    return Err(InferenceError::DifferentStackLengths(
                        stack_before,
                        stack.clone(),
                    ));
                }

                // Make sure that each type on the stack is the same as before the loop
                stack_before
                    .into_iter()
                    .zip(stack.clone().into_iter())
                    .for_each(|(t1, t2)| constraints.push(Constraint::Equal(t1, t2)));

                Ok(Expr::wile(
                    condition,
                    body,
                    (expr.meta, ConstrainedType::new(None, constraints)),
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
                BinOp::Eq | BinOp::Le | BinOp::Lt | BinOp::Ge | BinOp::Gt | BinOp::Ne => {
                    let right = stack.pop().ok_or_else(|| InferenceError::StackUnderflow)?;
                    let left = stack.pop().ok_or_else(|| InferenceError::StackUnderflow)?;
                    stack.push(Type::Bool);
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
                _ => todo!("{op:?} is not implemented yet"),
            },
            crate::expr::ExprBase::Call(f) => {
                let ty = env.get(&f).expect("Vars should be resolvable").clone();

                let func = self.freshen(ty, &mut HashMap::new()).expect_function();

                if stack.len() < func.input.len() {
                    todo!("not enough elements on the stack to call function");
                }

                let split = stack.split_off(stack.len() - func.input.len());
                let constraints = split
                    .into_iter()
                    .zip(func.input.clone().into_iter())
                    .map(|(left, right)| Constraint::Subtype {
                        parent: right,
                        child: left,
                    })
                    .collect::<Vec<_>>();
                stack.extend(func.output.clone());

                Ok(Expr::call(
                    f,
                    (
                        expr.meta,
                        ConstrainedType::new(Some(Type::Func(func)), constraints),
                    ),
                ))
            }
            crate::expr::ExprBase::If { then, otherwise } => {
                let condition = stack.pop().ok_or_else(|| InferenceError::StackUnderflow)?;

                let mut then_stack = stack.clone();
                let mut otherwise_stack = stack.clone();

                let then = self.infer(&mut then_stack, env, extern_fns, *then)?;
                let otherwise = self.infer(&mut otherwise_stack, env, extern_fns, *otherwise)?;

                if then_stack.len() != otherwise_stack.len() {
                    return Err(InferenceError::DifferentStackLengths(
                        then_stack,
                        otherwise_stack,
                    ));
                }

                *stack = otherwise_stack.clone();

                let mut constraints = vec![Constraint::Equal(condition, Type::Bool)];
                constraints.extend(
                    then_stack
                        .into_iter()
                        .zip(otherwise_stack.into_iter())
                        .map(|(left, right)| Constraint::Equal(left, right)),
                );

                Ok(Expr::iff(
                    then,
                    otherwise,
                    (expr.meta, ConstrainedType::new(None, constraints)),
                ))
            }
            crate::expr::ExprBase::Bind(n) => {
                if stack.len() < n {
                    return Err(InferenceError::StackUnderflow);
                }

                let popped = stack.split_off(stack.len() - n);
                let start = Self::env_get_num_idents(env);
                popped
                    .into_iter()
                    .enumerate()
                    .for_each(|(n, ty)| assert!(env.insert(Var::Ident(start + n), ty).is_none()));

                Ok(Expr::bind(n, (expr.meta, ConstrainedType::new(None, []))))
            }
            crate::expr::ExprBase::Var(var) => {
                let ty = env.get(&var).expect("vars should be resolveable");
                stack.push(ty.clone());

                Ok(Expr::var(
                    var,
                    (expr.meta, ConstrainedType::new(Some(ty.clone()), [])),
                ))
            }
            crate::expr::ExprBase::VarAccessor(var, accessors) => {
                let ty = env.get(&var).expect("vars should be resolveable");
                let ty = ty
                    .access_inner(&accessors)
                    .expect("TODO: Handle unknown inner value");

                stack.push(ty.clone());

                Ok(Expr::accessors(
                    var,
                    accessors,
                    (expr.meta, ConstrainedType::new(Some(ty.clone()), [])),
                ))
            }
            crate::expr::ExprBase::ExternCall(id) => {
                let extern_fn = extern_fns
                    .get(&id)
                    .expect("Extern functions should be resolvable")
                    .expect_function();

                let popped = extern_fn.take_inputs_from_stack(stack);

                let mut constraints = vec![];
                for (i, ty) in extern_fn.input.iter().enumerate() {
                    match ty {
                        CType::Variadic => break,
                        CType::Void => {
                            panic!("Can't accept a void type as argument")
                        }
                        c_ty => {
                            if let Some(ty) = popped.get(i) {
                                constraints
                                    .push(Constraint::Equal(c_ty.clone().into(), ty.clone()));
                            } else {
                                return Err(InferenceError::StackUnderflow);
                            }
                        }
                    }
                }

                match &*extern_fn.output {
                    CType::Void => (),
                    CType::Variadic => unreachable!(),
                    t => stack.push(t.clone().into()),
                }

                Ok(Expr::xtern(
                    id,
                    (expr.meta, ConstrainedType::new(None, constraints)),
                ))
            }
            crate::expr::ExprBase::Ext(_) => Err(InferenceError::ExtensionExpressionReached),
            crate::expr::ExprBase::Cast(ty) => {
                let target = self.freshen(ty, &mut HashMap::new());

                match target {
                    Type::U32 => todo!(),
                    Type::U8 => todo!(),
                    Type::Bool => todo!(),
                    Type::Var(_) => todo!(),
                    Type::Pointer { ty, mutable } => todo!(),
                    Type::Func(_) => todo!(),
                    Type::Struct(s) => {
                        if stack.len() < s.members.len() {
                            return Err(InferenceError::StackUnderflow);
                        }

                        let popped = stack.split_off(stack.len() - s.members.len());
                        let constraints = s
                            .members
                            .clone()
                            .into_iter()
                            .zip(popped.into_iter())
                            .map(|((_, expected), found)| Constraint::Equal(expected, found))
                            .collect::<Vec<_>>();

                        let t = Type::Struct(s);
                        stack.push(t.clone());

                        Ok(Expr::cast(
                            t.clone(),
                            (expr.meta, ConstrainedType::new(Some(t), constraints)),
                        ))
                    }
                    Type::TaggedUnion(_) => todo!(),
                    Type::Tuple(_) => todo!(),
                    Type::Never => todo!(),
                }
            }
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

    pub fn normalize_ty(&mut self, ty: Type) -> Type {
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
            Type::Func(f) => {
                // Type::Func {
                // input: input.into_iter().map(|ty| self.normalize_ty(ty)).collect(),
                // output: output.into_iter().map(|ty| self.normalize_ty(ty)).collect(),
                // },
                todo!()
            }
            Type::Struct(strukt) => Type::Struct(StructType {
                name: strukt.name,
                members: strukt
                    .members
                    .into_iter()
                    .map(|(ident, ty)| (ident, self.normalize_ty(ty)))
                    .collect(),
            }),
            Type::TaggedUnion(tu) => todo!(),
            Type::Tuple(_) => todo!(),
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
            ) => {
                // TODO: Handle variance for pointers
                self.unify_ty_ty(*left, *right, typing)
            }
            (Type::Struct(left), Type::Struct(right)) => {
                if left.name != right.name {
                    return Err(TypeUnificationError::NotEqual(
                        Type::Struct(left),
                        Type::Struct(right),
                    ));
                }

                left.members
                    .into_iter()
                    .zip(right.members.into_iter())
                    .map(|((_, left), (_, right))| self.unify_ty_ty(left, right, typing))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(())
            }
            (Type::Never, Type::Never) => Ok(()),
            (Type::Never, ty) | (ty, Type::Never) => {
                Err(TypeUnificationError::NotEqual(Type::Never, ty))
            }
            (left, right) => Err(TypeUnificationError::NotEqual(left, right)),
        }
    }

    fn substitute_one(&mut self, ty: Type) -> (HashSet<TypeVar>, Type) {
        match ty {
            Type::Never | Type::U32 | Type::U8 | Type::Bool => (HashSet::new(), ty),
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
            Type::Func(f) => {
                todo!()
                //     {
                //     let (in_subs, input) = self.substitute(input);
                //     let (out_subs, output) = self.substitute(output);

                //     (
                //         in_subs.into_iter().chain(out_subs.into_iter()).collect(),
                //         Type::Func { input, output },
                //     )
                // }
            }
            Type::Struct(strukt) => {
                let (subs, members) = strukt.members.into_iter().fold(
                    (HashSet::new(), Vec::new()),
                    |(mut substitutions, mut members), (id, ty)| {
                        let (subs, ty) = self.substitute_one(ty);
                        substitutions.extend(subs);

                        members.push((id, ty));
                        (substitutions, members)
                    },
                );

                (
                    subs,
                    Type::Struct(StructType {
                        name: strukt.name,
                        members,
                    }),
                )
            }
            Type::TaggedUnion(tu) => todo!(),
            Type::Tuple(_) => todo!(),
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
        expr: Expr<'a, (M, ConstrainedType), (), Type>,
    ) -> (HashSet<TypeVar>, Expr<'a, (M, ConstrainedType), (), Type>) {
        match expr.expr {
            crate::expr::ExprBase::Literal(_) => (HashSet::new(), expr),
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
            crate::expr::ExprBase::While { condition, body } => {
                let mut unbound = HashSet::new();
                let condition = condition
                    .into_iter()
                    .map(|e| self.substitute_expr(e))
                    .map(|(ub, e)| {
                        unbound.extend(ub);
                        e
                    })
                    .collect::<Vec<_>>();

                let (ub, body) = self.substitute_expr(*body);
                unbound.extend(ub);

                (unbound, Expr::wile(condition, body, expr.meta))
            }
            crate::expr::ExprBase::BinOp(_) => (HashSet::new(), expr),
            crate::expr::ExprBase::Call(Var::Func(f)) => {
                let func = expr
                    .meta
                    .1
                    .var_ty
                    .expect("Call exprs should have `var_ty`, but found none ")
                    .clone()
                    .expect_function();

                let (mut unbound, sub_input) = self.substitute(func.input);
                let (out_unbound, sub_output) = self.substitute(func.output);
                let (param_unbound, sub_params) = self.substitute(func.params);

                unbound.extend(out_unbound);
                unbound.extend(param_unbound);

                let fn_ty = Type::Func(FunctionType {
                    input: sub_input,
                    output: sub_output,
                    params: sub_params,
                });

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
            crate::expr::ExprBase::If { then, otherwise } => {
                let (mut unbound, sub_then) = self.substitute_expr(*then);
                let (unbound_otherwise, sub_otherwise) = self.substitute_expr(*otherwise);

                unbound.extend(unbound_otherwise);

                (unbound, Expr::iff(sub_then, sub_otherwise, expr.meta))
            }
            crate::expr::ExprBase::Bind(n) => (HashSet::new(), Expr::bind(n, expr.meta)),
            crate::expr::ExprBase::Var(var) => {
                let ty = expr
                    .meta
                    .1
                    .var_ty
                    .expect("vars should know what type they have been assigned to");
                let (unbound, ty) = self.substitute_one(ty);

                (
                    unbound,
                    Expr::var(var, (expr.meta.0, ConstrainedType::new(Some(ty), []))),
                )
            }
            crate::expr::ExprBase::VarAccessor(var, accessors) => {
                let ty = expr
                    .meta
                    .1
                    .var_ty
                    .expect("vars should know what type they have been assigned to");
                let (unbound, ty) = self.substitute_one(ty);

                (
                    unbound,
                    Expr::accessors(
                        var,
                        accessors,
                        (expr.meta.0, ConstrainedType::new(Some(ty), [])),
                    ),
                )
            }
            crate::expr::ExprBase::ExternCall(id) => (
                HashSet::new(),
                Expr::xtern(id, (expr.meta.0, ConstrainedType::new(None, []))),
            ),
            crate::expr::ExprBase::Ext(_) => unreachable!(),
            crate::expr::ExprBase::Cast(ty) => {
                let (unbound, ty) = self.substitute_one(ty);
                (unbound, Expr::cast(ty, expr.meta))
            }
        }
    }

    pub fn type_check<'a, M>(
        &mut self,
        expr: Expr<'a, M, (), Type>,
        parent_env: &HashMap<Var, Type>,
        extern_fns: &HashMap<usize, CType>,
        stack: &mut Stack,
        target: Stack,
        ty_vars_in_scope: Vec<&TypeVar>,
    ) -> Result<(Expr<'a, (M, ConstrainedType), (), Type>, Scheme), TypeCheckError>
    where
        M: Debug,
    {
        let (mut unbound, _) = self.substitute(stack.clone());
        let mut env = parent_env.clone();
        let mut out = self
            .infer(stack, &mut env, extern_fns, expr)
            .map_err(|e| match e {
                InferenceError::StackUnderflow => TypeCheckError::StackUnderflow,
                InferenceError::ExtensionExpressionReached => {
                    TypeCheckError::ExtensionExpressionReached
                }
                InferenceError::DifferentStackLengths(then, otherwise) => {
                    TypeCheckError::StacksDiffer(then, otherwise)
                }
            })?;

        out.meta
            .1
            .constriants
            .extend(Constraint::stack_compare(target, stack.clone())?);

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
    StacksDiffer(Stack, Stack),
}

#[cfg(test)]
mod inference_tests {}
