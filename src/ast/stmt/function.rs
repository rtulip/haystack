use crate::{
    ast::{
        arg::{TypedArg, UntypedArg},
        expr::Expr,
    },
    error::HayError,
    lex::token::Token,
    types::{Frame, FreeVars, FunctionType, Stack, Substitutions, TypeId},
};

use super::{Functions, InterfaceId, UserDefinedTypes};

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

pub struct FunctionDescription {
    pub name: Token,
    pub typ: FunctionType,
    pub body: Expr,
    pub tags: Vec<FnTag>,
    pub impl_on: Option<TypeId>,
    pub free_vars: Option<FreeVars>,
    pub start_state: (Stack, Frame),
}

impl FunctionStmt {
    pub fn add_to_global_env(
        self,
        user_defined_types: &UserDefinedTypes,
        functions: &mut Functions,
    ) -> Result<(), HayError> {
        let (free_vars, _) = UntypedArg::into_free_vars(self.annotations);
        let inputs = UntypedArg::into_typed_args(self.inputs, user_defined_types, &free_vars)?;
        let outputs = UntypedArg::into_typed_args(self.outputs, user_defined_types, &free_vars)?;

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
                body: self.body,
                tags: self.tags,
                impl_on: self.impl_on.map(|typ| TypeId::new(typ.lexeme)),
                free_vars,
                start_state: (stack, frame),
            },
        );

        Ok(())
    }
}

impl FunctionDescription {
    fn type_check(&self) -> Result<(), HayError> {
        let (mut stack, mut frame) = self.start_state.clone();
        self.body
            .type_check(&mut stack, &mut frame, &mut Substitutions::empty())
    }

    pub fn type_check_all(functions: &Functions) -> Result<(), HayError> {
        for (_, f) in functions {
            f.type_check()?;
        }
        Ok(())
    }
}
