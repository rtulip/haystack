use crate::{
    error::HayError,
    lex::token::Token,
    types::{Frame, Substitutions},
};

pub struct TypedGetFrameExpr {
    pub frame: Frame,
    pub idx: usize,
    pub inner: Option<Vec<String>>,
}

impl TypedGetFrameExpr {
    pub fn substitute(&mut self, _token: &Token, _subs: &Substitutions) -> Result<(), HayError> {
        Ok(())
    }
}
