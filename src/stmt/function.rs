use std::{collections::HashMap, fmt::Debug};

use crate::{
    c::CType,
    expr::Expr,
    generate,
    lex::LexType,
    passes::{Assignment, CSsaExtension},
    types::{ConstrainedType, Stack, Type, TypeCheckError, TypeInference, TypeVar, Var},
};

#[derive(Debug, Clone)]
pub struct TypeParameter<'src> {
    pub name: &'src str,
    pub tyvar: TypeVar,
}

#[derive(Clone)]
pub struct Function<'src, T, M, E> {
    pub name: &'src str,
    pub input: Vec<T>,
    pub output: Vec<T>,
    pub body: Expr<'src, M, E, T>,
    pub type_vars: Vec<TypeParameter<'src>>,
}

impl<'src, M> Function<'src, Type, M, ()> {
    pub fn type_check(
        self,
        inference: &mut TypeInference,
        parent_env: &HashMap<Var, Type>,
        extern_fns: &HashMap<usize, CType>,
    ) -> Result<Function<'src, Type, (M, ConstrainedType), ()>, (&'src str, TypeCheckError)>
    where
        M: Debug,
    {
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

    pub fn into_ssa_form(
        self,
        env: &HashMap<Var, Type>,
        extern_fns: &'src HashMap<usize, CType>,
        fn_names: &'src HashMap<usize, String>,
    ) -> Option<Function<'src, Type, Assignment, CSsaExtension<'src>>> {
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

impl<'src, T, M, E> Function<'src, T, M, E> {
    pub fn new<In, Out>(name: &'src str, input: In, output: Out, body: Expr<'src, M, E, T>) -> Self
    where
        In: Into<Vec<T>>,
        Out: Into<Vec<T>>,
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
        body: Expr<'src, M, E, T>,
        type_vars: Ts,
        ctx: &mut TypeInference,
    ) -> Self
    where
        In: Into<Vec<T>>,
        Out: Into<Vec<T>>,
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
}

impl<'src, M, E> Function<'src, LexType<'src>, M, E> {
    pub fn to_concrete_typed(
        self,
        types: &HashMap<&'src str, (Type, Vec<TypeParameter<'src>>)>,
    ) -> Function<'src, Type, M, E> {
        Function {
            name: self.name,
            input: self
                .input
                .into_iter()
                .map(|lex_ty| lex_ty.into_type(types, &self.type_vars).unwrap().0)
                .collect(),
            output: self
                .output
                .into_iter()
                .map(|lex_ty| lex_ty.into_type(types, &self.type_vars).unwrap().0)
                .collect(),
            body: self.body.to_concrete_typed(types, &self.type_vars),
            type_vars: self.type_vars,
        }
    }
}
