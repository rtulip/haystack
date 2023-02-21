use crate::{
    error::HayError,
    lex::token::Token,
    types::{Stack, Type},
};

use super::TypedExpr;

#[derive(Debug, Clone)]
pub struct NeverExpr {
    pub token: Token,
}

impl NeverExpr {
    pub fn type_check(&self, stack: &mut Stack) -> Result<TypedExpr, HayError> {
        stack.push(Type::Never.id());
        Ok(TypedExpr::Return)
    }
}
