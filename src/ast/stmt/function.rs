use std::collections::HashMap;

use crate::{
    ast::{
        arg::{TypedArg, UntypedArg},
        expr::{Expr, TypedAsExpr, TypedBlockExpr, TypedExpr},
    },
    error::HayError,
    lex::token::Token,
    types::{BaseType, Frame, FreeVars, FunctionType, Stack, Substitutions, Type, TypeId, TypeVar},
};

use super::{
    Functions, GlobalVars, InterfaceFunctionTable, InterfaceId, Interfaces, UserDefinedTypes,
};

#[derive(Debug, Clone)]
pub enum FnTag {
    Interface(InterfaceId),
    Inline,
}

#[derive(Debug, Clone)]
pub struct FunctionStmt {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<UntypedArg>,
    pub outputs: Vec<UntypedArg>,
    pub annotations: Option<Vec<UntypedArg>>,
    pub body: Expr,
    pub tags: Vec<FnTag>,
    pub impl_on: Option<Token>,
    pub requires: Option<Vec<Token>>,
}

#[derive(Debug, Clone)]
pub struct FunctionDescription {
    pub name: Token,
    pub typ: FunctionType,
    pub body: Option<Expr>,
    pub tags: Vec<FnTag>,
    pub impl_on: Option<TypeId>,
    pub free_vars: Option<FreeVars>,
    pub ordered_free_vars: Option<Vec<TypeVar>>,
    pub start_state: (Stack, Frame),
}

impl FunctionStmt {
    pub fn add_to_global_env(
        self,
        user_defined_types: &UserDefinedTypes,
        interfaces: &Interfaces,
        functions: &mut Functions,
        free_vars_in_scope: Option<&FreeVars>,
    ) -> Result<(), HayError> {
        let (free_vars, ordered_free_vars) = UntypedArg::into_free_vars(self.annotations);
        let inputs = UntypedArg::into_typed_args(
            self.inputs,
            user_defined_types,
            interfaces,
            Type::merge_free_vars(free_vars.as_ref(), free_vars_in_scope).as_ref(),
        )?;
        let outputs = UntypedArg::into_typed_args(
            self.outputs,
            user_defined_types,
            interfaces,
            Type::merge_free_vars(free_vars.as_ref(), free_vars_in_scope).as_ref(),
        )?;

        let (stack, frame) = TypedArg::init_state(&inputs);

        let function_type = FunctionType::from_typed_args(&inputs, &outputs);

        if self.requires.is_some() {
            todo!()
        }

        if functions.contains_key(&self.name.lexeme) {
            todo!()
        }

        functions.insert(
            self.name.lexeme.clone(),
            FunctionDescription {
                name: self.name,
                typ: function_type,
                body: Some(self.body),
                tags: self.tags,
                impl_on: self.impl_on.map(|typ| TypeId::new(typ.lexeme)),
                free_vars,
                ordered_free_vars,
                start_state: (stack, frame),
            },
        );

        Ok(())
    }
}

impl FunctionDescription {
    pub fn type_check(
        &self,
        global_vars: &GlobalVars,
        user_defined_types: &UserDefinedTypes,
        functions: &Functions,
        interfaces: &Interfaces,
        interface_fn_table: &InterfaceFunctionTable,
    ) -> Result<(TypedExpr, Token), HayError> {
        let (mut stack, mut frame) = self.start_state.clone();

        let mut wrapped_exprs = vec![];
        wrapped_exprs.push(TypedExpr::As(TypedAsExpr {
            typs: frame.clone().into_iter().map(|(_, t)| t).collect(),
        }));

        let mut typed_expr = self
            .body
            .as_ref()
            .ok_or(HayError::new(
                "Can't type check stub function...",
                self.name.loc.clone(),
            ))?
            .type_check(
                &mut stack,
                &mut frame,
                &self,
                user_defined_types,
                global_vars,
                functions,
                interfaces,
                interface_fn_table,
                self.free_vars.as_ref(),
                &mut Substitutions::empty(),
            )?;

        if self
            .typ
            .output
            .iter()
            .find(|t| t == &&Type::Base(BaseType::Never))
            .is_none()
            && stack.len() != self.typ.output.len()
        {
            todo!("{}", self.name)
        }

        let subs = FunctionType::new(self.typ.output.clone(), stack.clone())
            .unify(&self.name, &mut stack)?;

        if !subs.is_empty() {
            typed_expr.substitute(&self.name, &subs)?;
        }

        wrapped_exprs.push(typed_expr);

        Ok((
            TypedExpr::Block(TypedBlockExpr {
                exprs: vec![
                    TypedExpr::Block(TypedBlockExpr {
                        exprs: wrapped_exprs,
                    }),
                    TypedExpr::Return,
                ],
            }),
            self.name.clone(),
        ))
    }

    pub fn type_check_all(
        global_vars: &GlobalVars,
        functions: &Functions,
        user_defined_types: &UserDefinedTypes,
        interfaces: &mut Interfaces,
        interface_fn_table: &InterfaceFunctionTable,
    ) -> Result<HashMap<String, (TypedExpr, Token)>, HayError> {
        let mut typed_functions = HashMap::new();
        for (s, f) in functions {
            typed_functions.insert(
                s.clone(),
                f.type_check(
                    global_vars,
                    user_defined_types,
                    functions,
                    interfaces,
                    interface_fn_table,
                )?,
            );
        }

        for (_, iface) in interfaces.clone() {
            let fs = iface.type_check(
                global_vars,
                user_defined_types,
                functions,
                interfaces,
                interface_fn_table,
            )?;

            for (id, typed) in fs {
                assert!(typed_functions.insert(id, typed).is_none())
            }
        }

        Ok(typed_functions)
    }

    pub fn substitute(&mut self, token: &Token, subs: &Substitutions) -> Result<(), HayError> {
        self.typ = self.typ.clone().substitute(token, subs)?;
        for t in &mut self.start_state.0 {
            *t = t.clone().substitute(token, subs)?;
        }
        for (_, t) in &mut self.start_state.1 {
            *t = t.clone().substitute(token, subs)?;
        }
        Ok(())
    }
}
