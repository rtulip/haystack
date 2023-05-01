use crate::{error::HayError, lex::token::Token};

use super::{FunctionType, Stack, Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PointerType {
    pub mutable: bool,
    pub inner: Box<Type>,
}

impl PointerType {
    pub fn cast(&self, token: &Token, stack: &mut Stack) -> Result<(), HayError> {
        FunctionType::new(vec![Type::u64()], vec![Type::Pointer(self.clone())]).unify(token, stack)
    }
}
