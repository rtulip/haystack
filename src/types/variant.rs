use crate::{ast::expr::TypedExpr, error::HayError, lex::token::Token};

use super::{RecordKind, Stack, Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantType {
    pub variant: String,
    pub typ: Box<Type>,
}

impl VariantType {
    pub fn cast(&self, token: &Token, stack: &mut Stack) -> Result<TypedExpr, HayError> {
        match &self.typ {
            box Type::Record(ref record) => {
                if record.kind == RecordKind::EnumStruct {
                    record.cast_variant(&self.variant, token, stack)?;
                    todo!()
                } else {
                    todo!()
                }
            }
            _ => todo!(),
        }
    }
}
