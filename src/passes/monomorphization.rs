use std::{collections::HashMap, fmt::Debug};

use crate::{
    expr::Expr,
    stmt::Function,
    types::{ConstrainedType, Type, Var},
};

pub struct Monomorphized(bool, ConstrainedType);

impl Monomorphized {
    fn yes<M>((meta, t): (M, ConstrainedType)) -> (M, Self) {
        (meta, Monomorphized(true, t))
    }

    fn no<M>((meta, t): (M, ConstrainedType)) -> (M, Self) {
        (meta, Monomorphized(false, t))
    }
}

impl<'src, M> Expr<'src, (M, ConstrainedType), ()>
where
    M: Debug,
{
    fn find_monomorphizations(
        &mut self,
        env: &HashMap<Var, Type>,
        fn_idents: &mut HashMap<String, usize>,
        fn_names: &mut HashMap<usize, String>,
    ) -> Vec<(Var, usize, ConstrainedType)> {
        let mut to_monomorphise = vec![];
        match &mut self.expr {
            crate::expr::ExprBase::BinOp(_)
            | crate::expr::ExprBase::Bind(_)
            | crate::expr::ExprBase::Literal(_)
            | crate::expr::ExprBase::Print
            | crate::expr::ExprBase::PrintString
            | crate::expr::ExprBase::Var(_) => (),
            crate::expr::ExprBase::Block(ref mut exprs) => {
                to_monomorphise.extend(
                    exprs
                        .iter_mut()
                        .map(|e| e.find_monomorphizations(env, fn_idents, fn_names))
                        .flatten(),
                );
            }
            crate::expr::ExprBase::Call(func) => {
                if env.get(&func).unwrap().is_free() {
                    assert!(
                        !self.meta.1.is_free(),
                        "Generic Function calls should have been fully resolved"
                    );

                    let id = match &func {
                        Var::Func(id) => id,
                        _ => unimplemented!(),
                    };

                    let fn_ty = self
                        .meta
                        .1
                        .var_ty
                        .as_ref()
                        .unwrap()
                        .clone()
                        .expect_function();

                    let base_name = fn_names
                        .get(id)
                        .expect("Function names should be resolvable")
                        .clone();

                    let new_name = fn_ty
                        .params
                        .iter()
                        .fold(base_name, |base, ty| format!("{base}_{ty:?}"));

                    match fn_idents.get(&new_name) {
                        Some(id) => *func = Var::Func(*id),
                        None => {
                            assert!(fn_names.len() == fn_idents.len());
                            let id = fn_idents.len();

                            assert!(fn_names.insert(id, new_name.clone()).is_none());
                            assert!(fn_idents.insert(new_name.clone(), id).is_none());

                            to_monomorphise.push((func.clone(), id, self.meta.1.clone()));

                            *func = Var::Func(id)
                        }
                    }
                }
            }
            crate::expr::ExprBase::If { then, otherwise } => {
                to_monomorphise.extend(
                    [then, otherwise]
                        .into_iter()
                        .map(|e| e.find_monomorphizations(env, fn_idents, fn_names))
                        .flatten(),
                );
            }
            crate::expr::ExprBase::Ext(_) => unreachable!(),
        };

        to_monomorphise
    }
}

impl<'src, M> Function<'src, (M, ConstrainedType), ()>
where
    M: Debug,
{
    pub fn find_monomorphizations(
        &mut self,
        env: &HashMap<Var, Type>,
        fn_idents: &mut HashMap<String, usize>,
        fn_names: &mut HashMap<usize, String>,
    ) -> Vec<(Var, usize, ConstrainedType)> {
        self.body.find_monomorphizations(env, fn_idents, fn_names)
    }
}
