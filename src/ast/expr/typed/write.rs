use crate::{
    error::HayError,
    lex::token::Token,
    types::{Substitutions, Type},
};

pub struct TypedWriteExpr {
    pub typ: Type,
}

impl TypedWriteExpr {
    pub fn substitute(&mut self, token: &Token, subs: &Substitutions) -> Result<(), HayError> {
        self.typ = self.typ.clone().substitute(token, subs)?;
        Ok(())
    }
}
