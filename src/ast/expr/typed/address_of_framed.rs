use crate::{
    error::HayError,
    lex::token::Token,
    types::{Frame, Substitutions},
};

#[derive(Debug, Clone)]
pub struct TypedGetAddressOfFramedExpr {
    pub frame: Frame,
    pub idx: usize,
    pub inner: Option<Vec<String>>,
}

impl TypedGetAddressOfFramedExpr {
    pub fn substitute(&mut self, token: &Token, subs: &Substitutions) -> Result<(), HayError> {
        for (_, t) in &mut self.frame {
            *t = t.clone().substitute(token, subs)?;
        }
        Ok(())
    }
}
