use std::collections::HashMap;

use crate::{
    error::HayError,
    lex::token::{Literal, Token},
    types::{Signature, Stack, Type, TypeId, TypeMap, Variance},
};

use super::TypedExpr;

#[derive(Debug, Clone)]
pub struct ExprSizeOf {
    /// The token of the `sizeOf` keyword
    pub token: Token,
    /// The token of the type
    pub typ: Token,
}

impl ExprSizeOf {
    pub fn type_check(
        self,
        stack: &mut Stack,
        types: &mut TypeMap,
        generic_map: &Option<HashMap<TypeId, TypeId>>,
    ) -> Result<TypedExpr, HayError> {
        let tid = match TypeId::from_token(&self.typ, types, &vec![]) {
            Ok(tid) => tid,
            Err(_) => match generic_map {
                None => {
                    return Err(HayError::new_type_err(
                        format!("Cannot get the size of unknown type {}", self.typ.lexeme),
                        self.token.loc,
                    ));
                }
                Some(map) => {
                    let tid = TypeId::new(&self.typ.lexeme);

                    if let Some(tid) = map.get(&tid) {
                        tid.clone()
                    } else {
                        return Err(HayError::new_type_err(
                            format!("Cannot get size of unknown type: {}", self.typ.lexeme),
                            self.token.loc,
                        ));
                    }
                }
            },
        };

        Signature::new(vec![], vec![Type::U64.id()]).evaluate(
            &self.token,
            stack,
            types,
            Variance::Variant,
        )?;
        Ok(TypedExpr::Literal {
            value: Literal::U64((tid.size(types)? * tid.width()) as u64),
        })
    }
}
