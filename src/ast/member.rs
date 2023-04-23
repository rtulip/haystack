use std::collections::BTreeMap;

use crate::{
    error::HayError,
    lex::token::Token,
    types::{Type, TypeId},
};

use super::{stmt::UserDefinedTypes, visibility::Visitiliby};

#[derive(Debug, Clone)]
pub struct UntypedMember {
    pub parent: Option<Token>,
    pub vis: Visitiliby,
    pub token: Token,
    pub ident: Token,
}

impl UntypedMember {
    pub fn into_typed_member(
        self,
        user_defined_types: &UserDefinedTypes,
    ) -> Result<TypedMember, HayError> {
        Ok(TypedMember {
            parent: self
                .parent
                .as_ref()
                .map(|parent| TypeId::new(&parent.lexeme)),
            vis: self.vis,
            typ: Type::from_token(&self.token, user_defined_types)?,
            token: self.token,
            ident: self.ident,
        })
    }

    pub fn into_typed_members(
        members: Vec<Self>,
        user_defined_types: &UserDefinedTypes,
    ) -> Result<Vec<TypedMember>, HayError> {
        let mut out = vec![];

        for m in members {
            out.push(m.into_typed_member(user_defined_types)?);
        }

        Ok(out)
    }
}

#[derive(Debug, Clone)]
pub struct TypedMember {
    pub parent: Option<TypeId>,
    pub vis: Visitiliby,
    pub token: Token,
    pub ident: Token,
    pub typ: Type,
}

impl TypedMember {
    pub fn is_public(&self) -> bool {
        matches!(self.vis, Visitiliby::Public)
    }
}
