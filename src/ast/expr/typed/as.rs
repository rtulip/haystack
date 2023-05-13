use crate::{
    backend::Instruction,
    error::HayError,
    lex::token::Token,
    types::{Substitutions, Type},
};

use super::TypedCallExpr;

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

    pub fn into_instructions(&self) -> (Vec<Instruction>, Vec<TypedCallExpr>) {
        let mut instrs = vec![];
        let calls = vec![];
        for t in &self.typs {
            instrs.push(Instruction::PushToFrame {
                quad_words: t.size_unchecked(),
            })
        }

        (instrs, calls)
    }
}
