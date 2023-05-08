use crate::{error::HayError, lex::token::Token, types::Substitutions};

pub struct TypedGlobalExpr {
    pub ident: String,
}

impl TypedGlobalExpr {
    pub fn substitute(&mut self, _token: &Token, _subs: &Substitutions) -> Result<(), HayError> {
        Ok(())
    }
}
