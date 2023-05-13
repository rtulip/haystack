use crate::{
    backend::{InitDataMap, Instruction},
    error::HayError,
    lex::token::{Literal, Token},
    types::Substitutions,
};

use super::TypedCallExpr;

#[derive(Debug, Clone)]
pub struct TypedLiteralExpr {
    pub value: Literal,
}

impl TypedLiteralExpr {
    pub fn substitute(&mut self, _token: &Token, _subs: &Substitutions) -> Result<(), HayError> {
        Ok(())
    }

    pub fn into_instructions(
        &self,
        init_data: &mut InitDataMap,
    ) -> (Vec<Instruction>, Vec<TypedCallExpr>) {
        (Instruction::from_literal(&self.value, init_data), vec![])
    }
}
