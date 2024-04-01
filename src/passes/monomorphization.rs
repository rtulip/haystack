use std::{collections::HashMap, fmt::Debug};

use crate::{
    c::CType,
    expr::Expr,
    stmt::Function,
    types::{ConstrainedType, Type, TypeInference, Var},
};

impl<'src, M> Expr<'src, (M, ConstrainedType), ()>
where
    M: Debug,
{
    /// Given an expression find any generic function calls, and change them
    /// to a different concretely-typed function call.
    ///
    /// Returns a list containing the information required to create the new
    /// monomorphised function.
    ///     * Var: The key in `env` for the generic function to monomorphise.
    ///     * usize: The value to be used as a new `Var` for the new function.
    ///     * ConstrainedType: A Type::Function which the generic function will
    ///       be monomorphized into.
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
            | crate::expr::ExprBase::Var(_)
            | crate::expr::ExprBase::ExternCall { .. }
            | crate::expr::ExprBase::VarAccessor(_, _) => {
                // Non-function calls don't need any monomorphizations
            }
            crate::expr::ExprBase::Block(ref mut exprs) => {
                // Each sub-expression could have calls which need resolving
                to_monomorphise.extend(
                    exprs
                        .iter_mut()
                        .map(|e| e.find_monomorphizations(env, fn_idents, fn_names))
                        .flatten(),
                );
            }
            crate::expr::ExprBase::If { then, otherwise } => {
                // Each sub-expression could have calls which need resolving
                to_monomorphise.extend(
                    [then, otherwise]
                        .into_iter()
                        .map(|e| e.find_monomorphizations(env, fn_idents, fn_names))
                        .flatten(),
                );
            }
            crate::expr::ExprBase::While { condition, body } => {
                to_monomorphise.extend(
                    condition
                        .into_iter()
                        .map(|e| e.find_monomorphizations(env, fn_idents, fn_names))
                        .flatten(),
                );
                to_monomorphise.extend(body.find_monomorphizations(env, fn_idents, fn_names));
            }
            crate::expr::ExprBase::Call(func) => {
                // If the called function is free (i.e. has any free variables)
                // and this call expression has been type-checked into a function
                // type which is _not_ free, then we have enough information to
                // try and monomorphize this function.
                if env.get(&func).unwrap().is_free() && !self.meta.1.is_free() {
                    let id = func.func();

                    // Create a copy of the FunctionType which we need to resolve.
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

                    // Construct a new name for the monomorphized function from
                    // the monomorphized free types
                    let new_name = fn_ty
                        .params
                        .iter()
                        .fold(base_name, |base, ty| format!("{base}_{ty:?}"));

                    // Check to see if this function has alredy been monomorphized
                    //
                    // If so:
                    //    Modify this expression to point to the monomorphized fn.
                    // Otherwise:
                    //    Create a new function id
                    //    Insert the new name into the name lookup table
                    //    Insert the new id into the id lookup table
                    //    Add the info to the `to_monomorphize` list
                    //    Update this expression to point to the monomorphized fn.
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
            crate::expr::ExprBase::Ext(_) => unreachable!(),
        };

        to_monomorphise
    }
}

impl<'src, M> Function<'src, (M, ConstrainedType), ()>
where
    M: Debug,
{
    fn find_monomorphizations(
        &mut self,
        env: &HashMap<Var, Type>,
        fn_idents: &mut HashMap<String, usize>,
        fn_names: &mut HashMap<usize, String>,
    ) -> Vec<(Var, usize, ConstrainedType)> {
        self.body.find_monomorphizations(env, fn_idents, fn_names)
    }
}

pub fn find_monomorphizations<'a, 'src, M>(
    fns: &mut Vec<Function<'src, (M, ConstrainedType), ()>>,
    env: &HashMap<Var, Type>,
    fn_names: &mut HashMap<usize, String>,
    fn_idents: &mut HashMap<String, usize>,
) -> Vec<(Var, usize, ConstrainedType)>
where
    M: Debug,
{
    let to_monomorphise = fns
        .iter_mut()
        .map(|f| f.find_monomorphizations(&env, fn_idents, fn_names))
        .flatten()
        .collect::<Vec<_>>();

    to_monomorphise
}

pub fn resolve_monomorphizations<'src, M>(
    to_monomorphise: Vec<(Var, usize, ConstrainedType)>,
    mut fns: Vec<Function<'src, (M, ConstrainedType), ()>>,
    env: &mut HashMap<Var, Type>,
    extern_fns: &HashMap<usize, CType>,
    fn_names: &'src HashMap<usize, String>,
    inference: &mut TypeInference,
) -> Vec<Function<'src, (M, ConstrainedType), ()>>
where
    M: Debug + Clone,
{
    let monomorphized = to_monomorphise
        .into_iter()
        .map(|(var, new_idx, ty)| {
            let idx = *var.func();

            let fn_ty = ty.var_ty.unwrap().expect_function();

            // Create a clone of the function body
            let body = fns.get(idx).unwrap().body.clone();

            // Strip the type information from type checking from the body while
            //  preserving any other meta information
            let body = body.map_meta(|(m, _)| m);

            // Create a new function, with the new function name created from
            // `find_monomorphizations`.
            //
            // The type of this function comes from the ConstrainedType of the
            // original call site.
            let new_f: Function<'src, M, ()> = Function::new(
                fn_names.get(&new_idx).expect(
                    "This function id should have been created in `find_monomorphizations`",
                ),
                fn_ty.input,
                fn_ty.output,
                body,
            );

            // Add this function to the environment
            assert!(env.insert(Var::Func(new_idx), Type::from(&new_f)).is_none());
            new_f
        })
        .collect::<Vec<_>>();

    // Now we need to convert the monomorphized functions into the correct type.
    //
    // Every other function has gone through type checking at this point in
    // order to become monomorphized. This means we need to type check these new
    // functions as well.
    let monomorphized = monomorphized
        .into_iter()
        .map(|func| {
            func.type_check(inference, &env, extern_fns)
                .expect("Monomorphized functions should type check")
        })
        .collect::<Vec<_>>();

    // Add all the new functions to the list of all functions.
    fns.extend(monomorphized);
    fns
}
