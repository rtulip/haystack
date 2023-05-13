use crate::{error::HayError, lex::token::Token, types::Substitutions};

use super::TypedExpr;

#[derive(Debug, Clone)]
pub struct TypedWhileExpr {
    pub cond: Vec<TypedExpr>,
    pub body: Box<TypedExpr>,
}

impl TypedWhileExpr {
    pub fn substitute(&mut self, token: &Token, subs: &Substitutions) -> Result<(), HayError> {
        for e in &mut self.cond {
            e.substitute(token, subs)?;
        }

        self.body.substitute(token, subs)
    }
}
