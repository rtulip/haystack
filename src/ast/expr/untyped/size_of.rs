use std::collections::HashMap;

use crate::{
    error::HayError,
    lex::token::{Literal, Token},
    types::{Stack, Type},
};

#[derive(Debug, Clone)]
pub struct SizeOfExpr {
    /// The token of the `sizeOf` keyword
    pub token: Token,
    /// The token of the type
    pub typ: Token,
}

impl SizeOfExpr {
    pub fn type_check(&self, stack: &mut Stack) -> Result<(), HayError> {
        stack.push(Type::u64());

        Ok(())
    }
}
