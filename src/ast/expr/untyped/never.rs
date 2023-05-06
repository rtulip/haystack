use crate::{
    error::HayError,
    lex::token::Token,
    types::{Stack, Type},
};

#[derive(Debug, Clone)]
pub struct NeverExpr {
    pub token: Token,
}

impl NeverExpr {
    pub fn type_check(&self, stack: &mut Stack) -> Result<(), HayError> {
        stack.push(Type::never());
        Ok(())
    }
}
