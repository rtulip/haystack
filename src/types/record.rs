use std::fmt::Display;

use crate::{
    ast::{
        expr::{TypedCastExpr, TypedExpr},
        visibility::Visibility,
    },
    error::HayError,
    lex::token::Token,
};

use super::{FreeVars, FunctionType, Stack, Substitutions, Type, TypeId, TypeVar, VariantType};

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
    pub ordered_free_vars: Option<Vec<Type>>,
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
            ordered_free_vars: None,
        }
    }

    pub fn get_inner_accessors(&self, accessors: &Vec<Token>) -> Result<Self, HayError> {
        assert!(self.kind == RecordKind::Struct);

        if accessors.is_empty() {
            return Ok(self.clone());
        }

        todo!()
    }

    pub fn cast(&self, token: &Token, stack: &mut Stack) -> Result<TypedExpr, HayError> {
        match self.kind {
            RecordKind::Struct => {
                let inputs = self
                    .members
                    .iter()
                    .map(|member| member.typ.clone())
                    .collect();
                let f = FunctionType::new(inputs, vec![Type::Record(self.clone())]);

                let subs = f.unify(token, stack)?;
                stack
                    .iter_mut()
                    .rev()
                    .take(f.output.len())
                    .for_each(|t| *t = t.clone().substitute(token, &subs).unwrap());
                Ok(TypedExpr::Cast(TypedCastExpr {
                    typ: Type::Record(self.clone()),
                }))
            }
            _ => todo!("{:?}", self.kind),
        }
    }

    pub fn cast_variant(
        &self,
        variant: &String,
        token: &Token,
        stack: &mut Stack,
    ) -> Result<TypedExpr, HayError> {
        assert!(self.kind == RecordKind::EnumStruct);

        if let Some(member) = self.members.iter().find(|member| &member.ident == variant) {
            let variant = Type::Variant(VariantType {
                variant: variant.clone(),
                typ: Box::new(Type::Record(self.clone())),
            });
            let f = FunctionType::new(vec![member.typ.clone()], vec![variant.clone()]);

            f.unify(token, stack)?;

            Ok(TypedExpr::Cast(TypedCastExpr { typ: variant }))
        } else {
            todo!()
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
