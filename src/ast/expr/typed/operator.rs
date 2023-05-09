use crate::{
    error::HayError,
    lex::token::{Operator, Token},
    types::Substitutions,
};

#[derive(Debug, Clone)]
pub struct TypedOperatorExpr {
    pub op: Operator,
}

impl TypedOperatorExpr {
    pub fn substitute(&mut self, _token: &Token, _subs: &Substitutions) -> Result<(), HayError> {
        Ok(())
    }
}
