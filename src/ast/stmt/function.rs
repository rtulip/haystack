use crate::{
    ast::{
        arg::{TypedArg, UntypedArg},
        expr::Expr,
    },
    error::HayError,
    lex::token::Token,
    types::{Frame, FreeVars, FunctionType, Stack, Substitutions, Type, TypeId, TypeVar},
};

use super::{Functions, InterfaceFunctionTable, InterfaceId, Interfaces, UserDefinedTypes};

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
        functions: &mut Functions,
        free_vars_in_scope: Option<&FreeVars>,
    ) -> Result<(), HayError> {
        let (free_vars, ordered_free_vars) = UntypedArg::into_free_vars(self.annotations);
        let inputs = UntypedArg::into_typed_args(
            self.inputs,
            user_defined_types,
            Type::merge_free_vars(free_vars.as_ref(), free_vars_in_scope).as_ref(),
        )?;
        let outputs = UntypedArg::into_typed_args(
            self.outputs,
            user_defined_types,
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
        types: &UserDefinedTypes,
        functions: &Functions,
        interfaces: &Interfaces,
        interface_fn_table: &InterfaceFunctionTable,
    ) -> Result<(), HayError> {
        println!("{}: Type Checking {}", self.name.loc, self.name.lexeme);
        let (mut stack, mut frame) = self.start_state.clone();
        self.body
            .as_ref()
            .ok_or(HayError::new(
                "Can't type check stub function...",
                self.name.loc.clone(),
            ))?
            .type_check(
                types,
                &mut stack,
                &mut frame,
                functions,
                interfaces,
                interface_fn_table,
                &mut Substitutions::empty(),
            )
    }

    pub fn type_check_all(
        functions: &Functions,
        types: &UserDefinedTypes,
        interfaces: &Interfaces,
        interface_fn_table: &InterfaceFunctionTable,
    ) -> Result<(), HayError> {
        for (_, f) in functions {
            f.type_check(types, functions, interfaces, interface_fn_table)?;
        }
        Ok(())
    }
}
