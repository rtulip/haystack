use crate::{error::HayError, lex::token::Token, types::Substitutions};

use super::TypedExpr;

#[derive(Debug, Clone)]
pub struct TypedIfExpr {
    pub then: Box<TypedExpr>,
    pub otherwise: Vec<TypedElseIfExpr>,
    pub finally: Option<Box<TypedExpr>>,
}

#[derive(Debug, Clone)]
pub struct TypedElseIfExpr {
    pub condition: Vec<TypedExpr>,
    pub block: Box<TypedExpr>,
}

impl TypedIfExpr {
    pub fn substitute(&mut self, token: &Token, subs: &Substitutions) -> Result<(), HayError> {
        self.then.substitute(token, subs)?;
        for e in &mut self.otherwise {
            e.substitute(token, subs)?;
        }

        if let Some(finally) = &mut self.finally {
            finally.substitute(token, subs)?;
        }

        Ok(())
    }
}

impl TypedElseIfExpr {
    pub fn substitute(&mut self, token: &Token, subs: &Substitutions) -> Result<(), HayError> {
        for e in &mut self.condition {
            e.substitute(token, subs)?;
        }
        self.block.substitute(token, subs)
    }
}
