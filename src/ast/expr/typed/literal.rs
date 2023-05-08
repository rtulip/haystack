use crate::{
    error::HayError,
    lex::token::{Literal, Token},
    types::Substitutions,
};

pub struct TypedLiteralExpr {
    pub value: Literal,
}

impl TypedLiteralExpr {
    pub fn substitute(&mut self, _token: &Token, _subs: &Substitutions) -> Result<(), HayError> {
        Ok(())
    }
}
