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
    ) -> Result<(), HayError> {
        let cast_type = Type::from_token(&self.typ, user_defined_types, &mut FreeVars::new())?;

        match cast_type {
            Type::Base(base_type) => base_type.unify_cast(&self.token, stack),
            Type::Record(record) => record.unify_cast(&self.token, stack),
            Type::Pointer(pointer) => pointer.unify_cast(&self.token, stack),
            _ => todo!("{cast_type}"),
        }
    }
}
