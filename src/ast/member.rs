use std::collections::BTreeMap;

use crate::{
    error::HayError,
    lex::token::Token,
    types::{Type, TypeId},
};

use super::visibility::Visitiliby;

#[derive(Debug, Clone)]
pub struct UntypedMember {
    pub parent: Option<Token>,
    pub vis: Visitiliby,
    pub token: Token,
    pub ident: Token,
}

impl UntypedMember {}

#[derive(Debug, Clone)]
pub struct TypedMember {
    pub parent: Option<TypeId>,
    pub vis: Visitiliby,
    pub token: Token,
    pub ident: Token,
    pub typ: TypeId,
}

impl TypedMember {
    pub fn is_public(&self) -> bool {
        matches!(self.vis, Visitiliby::Public)
    }
}
