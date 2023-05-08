use crate::{error::HayError, lex::token::Token, types::Substitutions};

use super::TypedExpr;

pub struct TypedBlockExpr {
    pub exprs: Vec<TypedExpr>,
}

impl TypedBlockExpr {
    pub fn substitute(&mut self, token: &Token, subs: &Substitutions) -> Result<(), HayError> {
        for e in &mut self.exprs {
            e.substitute(token, subs)?;
        }

        Ok(())
    }
}
