use std::collections::BTreeMap;

use crate::{
    error::HayError,
    lex::token::Token,
    types::{Type, TypeId},
};

use super::visibility::Visitiliby;

#[derive(Debug, Clone)]
pub struct UntypedMember {
    pub parent: Token,
    pub vis: Visitiliby,
    pub token: Token,
    pub ident: Token,
}

impl UntypedMember {
    pub fn resolve(
        members: Vec<Self>,
        types: &mut BTreeMap<TypeId, Type>,
        local_types: &Vec<TypeId>,
    ) -> Result<Vec<TypedMember>, HayError> {
        let mut out = vec![];
        for m in members {
            let typ = TypeId::from_token(&m.token, types, local_types)?;

            let parent = TypeId::new(&m.parent.lexeme);
            out.push(TypedMember {
                parent,
                vis: m.vis,
                token: m.token,
                ident: m.ident,
                typ,
            })
        }

        Ok(out)
    }
}

#[derive(Debug, Clone)]
pub struct TypedMember {
    pub parent: TypeId,
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
