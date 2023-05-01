use std::fmt::Display;

use crate::{ast::visibility::Visibility, error::HayError, lex::token::Token};

use super::{FunctionType, Stack, Type, TypeId, TypeVar};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecordKind {
    Enum,
    Struct,
    EnumStruct,
    Tuple,
    Union,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordMember {
    pub ident: String,
    pub vis: Visibility,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordType {
    pub kind: RecordKind,
    pub ident: Option<TypeId>,
    pub members: Vec<RecordMember>,
}

impl RecordType {
    pub fn tuple(members: Vec<Type>) -> Self {
        let kind = RecordKind::Tuple;
        RecordType {
            kind,
            ident: None,
            members: members
                .into_iter()
                .enumerate()
                .map(|(i, t)| RecordMember {
                    ident: format!("{i}"),
                    vis: Visibility::Public,
                    typ: t,
                })
                .collect(),
        }
    }

    pub fn get_inner_accessors(&self, accessors: &Vec<Token>) -> Result<Self, HayError> {
        assert!(self.kind == RecordKind::Struct);

        if accessors.is_empty() {
            return Ok(self.clone());
        }

        todo!()
    }

    pub fn unify_cast(&self, token: &Token, stack: &mut Stack) -> Result<(), HayError> {
        match self.kind {
            RecordKind::Struct => {
                let inputs = self
                    .members
                    .iter()
                    .map(|member| member.typ.clone())
                    .collect();
                let f = FunctionType::new(inputs, vec![Type::Record(self.clone())]);

                f.unify(token, stack)
            }
            _ => todo!("{:?}", self.kind),
        }
    }
}

impl Display for RecordKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RecordKind::Enum => write!(f, "enum"),
            RecordKind::Struct => write!(f, "struct"),
            RecordKind::EnumStruct => write!(f, "enum struct"),
            RecordKind::Tuple => write!(f, "tuple"),
            RecordKind::Union => write!(f, "union"),
        }
    }
}
