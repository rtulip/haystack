use crate::{
    error::HayError,
    lex::token::{Literal, Token},
    types::{Stack, Type, TypeId},
};

use super::TypedExpr;

#[derive(Debug, Clone)]
pub struct ExprLiteral {
    pub literal: Literal,
    pub token: Token,
}

impl ExprLiteral {
    pub fn type_check(self, stack: &mut Stack) -> Result<TypedExpr, HayError> {
        match &self.literal {
            Literal::Bool(_) => stack.push(Type::Bool.id()),
            Literal::Char(_) => stack.push(Type::Char.id()),
            Literal::U64(_) => stack.push(Type::U64.id()),
            Literal::U8(_) => stack.push(Type::U8.id()),
            Literal::String(_) => stack.push(TypeId::new("Str")),
        }

        Ok(TypedExpr::Literal {
            value: self.literal,
        })
    }
}
