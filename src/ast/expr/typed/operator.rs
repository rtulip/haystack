use crate::{
    backend::Instruction,
    error::HayError,
    lex::token::{Operator, Token},
    types::Substitutions,
};

use super::TypedCallExpr;

#[derive(Debug, Clone)]
pub struct TypedOperatorExpr {
    pub op: Operator,
}

impl TypedOperatorExpr {
    pub fn substitute(&mut self, _token: &Token, _subs: &Substitutions) -> Result<(), HayError> {
        Ok(())
    }

    pub fn into_instructions(&self) -> (Vec<Instruction>, Vec<TypedCallExpr>) {
        (
            vec![Instruction::Operator {
                op: self.op.clone(),
                size: None,
            }],
            vec![],
        )
    }
}
