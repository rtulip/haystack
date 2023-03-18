use crate::{
    error::HayError,
    lex::token::Token,
    types::{Stack, Type, TypeMap},
};

use super::TypedExpr;

#[derive(Debug, Clone)]
pub struct UnpackExpr {
    pub token: Token,
}

impl UnpackExpr {
    pub fn type_check(&self, stack: &mut Stack, types: &TypeMap) -> Result<TypedExpr, HayError> {
        let typ =
            match stack.pop() {
                Some(t) => t,
                None => return Err(HayError::new(
                    "`Unpack` expression requires a tuple on top of the stack, but stack was empty",
                    self.token.loc.clone(),
                )),
            };

        if let Some(Type::Tuple { inner, .. }) = types.get(&typ) {
            inner.iter().for_each(|t| stack.push(t.clone()));
        } else {
            return Err(HayError::new(
                format!("`Unpack` expression requires a tuple on top of the stack, found `{typ}` instead."), 
                self.token.loc.clone()
            ));
        }

        Ok(TypedExpr::Unpack { typ })
    }
}
