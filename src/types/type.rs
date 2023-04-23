use crate::{
    error::HayError,
    lex::token::{Token, TokenKind, TypeToken},
};

use super::{BaseType, FunctionType, PointerType, RecordType, TypeVar};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Base(BaseType),
    Pointer(PointerType),
    TypeVar(TypeVar),
    Record(RecordType),
    Function(FunctionType),
}

impl Type {
    pub fn from_token(token: &Token) -> Result<Self, HayError> {
        let typ = match &token.kind {
            TokenKind::Type(typ) => typ,
            _ => {
                return Err(HayError::new(
                    format!(
                        "Cannot extract type from {}, as it is not a type token!",
                        token.lexeme
                    ),
                    token.loc.clone(),
                ))
            }
        };

        Self::from_type_token(token, typ)
    }

    pub fn from_type_token(token: &Token, typ: &TypeToken) -> Result<Self, HayError> {
        match typ {
            TypeToken::Array { base, size } => todo!(),
            TypeToken::Associated { base, typ } => todo!(),
            TypeToken::Base(base) => Ok(Type::Base(BaseType::try_from(base.as_ref())?)),
            TypeToken::Parameterized { base, inner } => todo!(),
            TypeToken::Pointer { inner, mutable } => Ok(Type::Pointer(PointerType {
                mutable: *mutable,
                inner: Box::new(Type::from_type_token(token, inner)?),
            })),
            TypeToken::Tuple { inner, idents } => todo!(),
        }
    }
}
