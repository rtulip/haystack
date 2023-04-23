use crate::{
    ast::stmt::UserDefinedTypes,
    error::HayError,
    lex::token::{Token, TokenKind, TypeToken},
};

use super::{BaseType, FunctionType, PointerType, RecordType, TypeId, TypeVar};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Base(BaseType),
    Pointer(PointerType),
    TypeVar(TypeVar),
    Record(RecordType),
    Function(FunctionType),
}

impl Type {
    pub fn from_token(
        token: &Token,
        user_defined_types: &UserDefinedTypes,
    ) -> Result<Self, HayError> {
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

        Self::from_type_token(token, typ, user_defined_types)
    }

    pub fn from_type_token(
        token: &Token,
        typ: &TypeToken,
        user_defined_types: &UserDefinedTypes,
    ) -> Result<Self, HayError> {
        match typ {
            TypeToken::Array { base, size } => todo!(),
            TypeToken::Associated { base, typ } => todo!(),
            TypeToken::Base(base) => {
                if let Ok(base) = BaseType::try_from(base.as_ref()) {
                    return Ok(Type::Base(base));
                }
                let tid = TypeId::new(base);
                if let Some(desc) = user_defined_types.get(&tid) {
                    return Ok(Type::Record(desc.typ.clone()));
                }

                todo!()
            }
            TypeToken::Parameterized { base, inner } => todo!(),
            TypeToken::Pointer { inner, mutable } => Ok(Type::Pointer(PointerType {
                mutable: *mutable,
                inner: Box::new(Type::from_type_token(token, inner, user_defined_types)?),
            })),
            TypeToken::Tuple { inner, idents } => todo!(),
        }
    }
}
