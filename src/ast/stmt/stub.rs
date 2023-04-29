use crate::{
    ast::arg::{TypedArg, UntypedArg},
    error::HayError,
    lex::token::Token,
    types::{FreeVars, FunctionType, Type, TypeId},
};

use super::{FnTag, FunctionDescription, Functions, Stmt, StmtKind, UserDefinedTypes};

#[derive(Debug, Clone)]
pub struct FunctionStubStmt {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<UntypedArg>,
    pub outputs: Vec<UntypedArg>,
    pub annotations: Option<Vec<UntypedArg>>,
    pub impl_on: Option<Token>,
    pub tags: Vec<FnTag>,
    pub requires: Option<Vec<Token>>,
}

impl FunctionStubStmt {
    pub fn add_to_global_env(
        self,
        user_defined_types: &UserDefinedTypes,
        functions: &mut Functions,
        free_vars_in_scope: Option<&FreeVars>,
    ) -> Result<(), HayError> {
        let (free_vars, _) = UntypedArg::into_free_vars(self.annotations);
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
                body: None,
                tags: self.tags,
                impl_on: self.impl_on.map(|typ| TypeId::new(typ.lexeme)),
                free_vars,
                start_state: (stack, frame),
            },
        );

        Ok(())
    }
}
