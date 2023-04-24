use std::collections::HashMap;

use crate::{
    error::HayError,
    lex::token::Token,
    types::{Frame, Stack, Substitutions},
};

use super::Expr;

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub open: Token,
    pub close: Token,
    pub exprs: Vec<Expr>,
}

impl BlockExpr {
    pub fn type_check(
        &self,
        stack: &mut Stack,
        frame: &mut Frame,
        subs: &mut Substitutions,
    ) -> Result<(), HayError> {
        for e in &self.exprs {
            e.type_check(stack, frame, subs)?;
        }

        Ok(())
    }
}
