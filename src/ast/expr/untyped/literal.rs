use crate::{
    ast::{
        expr::{TypedExpr, TypedLiteralExpr},
        stmt::{TypeDescription, UserDefinedTypes},
    },
    backend::{InitData, InitDataMap, Instruction},
    error::HayError,
    lex::token::{Literal, Token},
    types::{Stack, Type, TypeId},
};

#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub literal: Literal,
    pub token: Token,
}

impl LiteralExpr {
    pub fn type_check(
        &self,
        stack: &mut Stack,
        types: &UserDefinedTypes,
    ) -> Result<TypedExpr, HayError> {
        match self.literal {
            Literal::Bool(_) => stack.push(Type::bool()),
            Literal::Char(_) => stack.push(Type::char()),
            Literal::String(_) => {
                let typ = types.get(&TypeId::new("Str")).unwrap();
                match typ {
                    TypeDescription::Record(record) => stack.push(Type::Record(record.typ.clone())),
                    &TypeDescription::PreDeclaration(_) => todo!(),
                }
            }
            Literal::U64(_) => stack.push(Type::u64()),
            Literal::U8(_) => stack.push(Type::u8()),
        }

        Ok(TypedExpr::Literal(TypedLiteralExpr {
            value: self.literal.clone(),
        }))
    }
}