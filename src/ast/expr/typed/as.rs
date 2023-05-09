use crate::{
    error::HayError,
    lex::token::Token,
    types::{Substitutions, Type},
};

#[derive(Debug, Clone)]
pub struct TypedAsExpr {
    pub typs: Vec<Type>,
}

impl TypedAsExpr {
    pub fn substitute(&mut self, token: &Token, subs: &Substitutions) -> Result<(), HayError> {
        for t in &mut self.typs {
            *t = t.clone().substitute(token, subs)?;
        }

        Ok(())
    }
}
