use std::collections::HashMap;

use crate::{
    ast::{
        expr::TypedExpr,
        member::TypedMember,
        stmt::{FunctionDescription, Interfaces, UserDefinedTypes},
    },
    error::HayError,
    lex::token::Token,
    types::{FreeVars, Stack, Type},
};

#[derive(Debug, Clone)]
pub struct CastExpr {
    pub token: Token,
    pub typ: Token,
}

impl CastExpr {
    pub fn type_check(
        &self,
        stack: &mut Stack,
        user_defined_types: &UserDefinedTypes,
        interfaces: &Interfaces,
        function: &FunctionDescription,
    ) -> Result<TypedExpr, HayError> {
        let cast_type = Type::from_token(
            &self.typ,
            user_defined_types,
            interfaces,
            function.free_vars.as_ref(),
        )?;
        cast_type.cast(&self.token, stack)
    }
}
