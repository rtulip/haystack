use crate::{error::HayError, lex::token::Token, types::Substitutions};

#[derive(Debug, Clone)]
pub struct TypedGlobalExpr {
    pub ident: String,
}

impl TypedGlobalExpr {
    pub fn substitute(&mut self, _token: &Token, _subs: &Substitutions) -> Result<(), HayError> {
        Ok(())
    }
}
