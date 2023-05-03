use std::collections::HashMap;

use crate::{
    ast::{member::TypedMember, stmt::UserDefinedTypes},
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
        free_vars: &FreeVars,
    ) -> Result<(), HayError> {
        let cast_type = Type::from_token(&self.typ, user_defined_types, free_vars)?;
        cast_type.cast(&self.token, stack)
    }
}
