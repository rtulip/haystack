use crate::{
    backend::{InitDataMap, Instruction},
    error::HayError,
    lex::token::Token,
    types::Substitutions,
};

use super::{TypedCallExpr, TypedExpr};

#[derive(Debug, Clone)]
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

    pub fn into_instructions(
        &self,
        init_data: &mut InitDataMap,
    ) -> (Vec<Instruction>, Vec<TypedCallExpr>) {
        let mut instrs = vec![];
        let mut calls = vec![];
        for e in &self.exprs {
            let (temp_instrs, temp_calls) = e.into_instructions(init_data);
            instrs.extend(temp_instrs);
            calls.extend(temp_calls);
        }

        (instrs, calls)
    }
}
