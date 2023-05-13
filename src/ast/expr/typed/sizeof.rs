use crate::{
    error::HayError,
    lex::token::Token,
    types::{Substitutions, Type},
};

#[derive(Debug, Clone)]
pub struct TypedSizeOfExpr {
    pub typ: Type,
}

impl TypedSizeOfExpr {
    pub fn substitute(&mut self, token: &Token, subs: &Substitutions) -> Result<(), HayError> {
        self.typ = self.typ.clone().substitute(token, subs)?;
        Ok(())
    }
}
