use std::collections::HashMap;

use crate::{
    ast::{
        expr::{TypedExpr, TypedLiteralExpr, TypedSizeOfExpr},
        stmt::{Interfaces, UserDefinedTypes},
    },
    error::HayError,
    lex::token::{Literal, Token},
    types::{FreeVars, Stack, Type},
};

#[derive(Debug, Clone)]
pub struct SizeOfExpr {
    /// The token of the `sizeOf` keyword
    pub token: Token,
    /// The token of the type
    pub typ: Token,
}

impl SizeOfExpr {
    pub fn type_check(
        &self,
        stack: &mut Stack,
        user_defined_types: &UserDefinedTypes,
        interfaces: &Interfaces,
        free_vars: Option<&FreeVars>,
    ) -> Result<TypedExpr, HayError> {
        stack.push(Type::u64());
        let typ = Type::from_token(&self.typ, user_defined_types, interfaces, free_vars)?;

        match typ.size(&self.token) {
            Ok(size) => Ok(TypedExpr::Literal(TypedLiteralExpr {
                value: Literal::U64(size as u64),
            })),
            Err(_) => Ok(TypedExpr::SizeOf(TypedSizeOfExpr { typ })),
        }
    }
}
