use std::collections::HashMap;

use crate::{
    c::CType,
    expr::Expr,
    generate,
    passes::{Assignment, CSsaExtension},
    types::{ConstrainedType, Stack, Type, TypeCheckError, TypeInference, TypeVar, Var},
};

#[derive(Debug, Clone)]
pub struct TypeParameter<'src> {
    name: &'src str,
    pub tyvar: TypeVar,
}

#[derive(Clone)]
pub struct Function<'src, M, E> {
    pub name: &'src str,
    pub input: Stack,
    pub output: Stack,
    pub body: Expr<'src, M, E>,
    pub type_vars: Vec<TypeParameter<'src>>,
}

impl<'src, M> Function<'src, M, ()> {
    pub fn type_check(
        self,
        inference: &mut TypeInference,
        parent_env: &HashMap<Var, Type>,
        extern_fns: &HashMap<usize, CType>,
    ) -> Result<Function<'src, (M, ConstrainedType), ()>, (&'src str, TypeCheckError)> {
        let (e, _scheme) = inference
            .type_check(
                self.body,
                parent_env,
                extern_fns,
                &mut self.input.clone(),
                self.output.clone(),
                self.type_vars.iter().map(|param| &param.tyvar).collect(),
            )
            .map_err(|e| (self.name, e))?;

        // TODO: there's stuff that needs to be checked with the scheme here
        // once we've got generic functions

        Ok(Function {
            name: self.name,
            input: self.input,
            output: self.output,
            body: e,
            type_vars: self.type_vars,
        })
    }
}

impl<'src, M, E> Function<'src, M, E> {
    pub fn new<In, Out>(name: &'src str, input: In, output: Out, body: Expr<'src, M, E>) -> Self
    where
        In: Into<Stack>,
        Out: Into<Stack>,
    {
        Function {
            name,
            input: input.into(),
            output: output.into(),
            body,
            type_vars: vec![],
        }
    }

    pub fn new_generic<In, Out, Ts>(
        name: &'src str,
        input: In,
        output: Out,
        body: Expr<'src, M, E>,
        type_vars: Ts,
        ctx: &mut TypeInference,
    ) -> Self
    where
        In: Into<Stack>,
        Out: Into<Stack>,
        Ts: Into<Vec<&'src str>>,
    {
        let type_vars = type_vars
            .into()
            .into_iter()
            .map(|name| TypeParameter {
                name,
                tyvar: ctx.fresh_type_var(),
            })
            .collect();

        Function {
            name,
            input: input.into(),
            output: output.into(),
            body,
            type_vars,
        }
    }

    pub fn into_ssa_form(
        self,
        env: &HashMap<Var, Type>,
        extern_fns: &'src HashMap<usize, CType>,
        fn_names: &'src HashMap<usize, String>,
    ) -> Option<Function<'src, Assignment, CSsaExtension<'src>>> {
        if !self.type_vars.is_empty() {
            return None;
        }

        let mut body = self
            .body
            .into_ssa_form(self.input.clone(), env, extern_fns, fn_names);

        if !self.output.is_empty() {
            let ty = CType::from(
                self.output
                    .iter()
                    .map(|ty| CType::from(ty.clone()))
                    .collect::<Vec<_>>(),
            );

            let ret = Expr::ext(
                CSsaExtension::Return(ty),
                Assignment {
                    input: Some(body.meta.output.as_ref().unwrap().clone()),
                    output: None,
                },
            );

            body = Expr::block(
                [body, ret],
                Assignment {
                    input: None,
                    output: None,
                },
            );
        }

        Some(Function::new(self.name, self.input, self.output, body))
    }
}
