use std::collections::BTreeMap;

use crate::{
    error::HayError,
    lex::token::Token,
    types::{Type, TypeId, Typed},
};

use super::visibility::Visitiliby;

#[derive(Debug, Clone)]
pub struct Member<TypeState> {
    pub vis: Visitiliby,
    pub token: Token,
    pub ident: Token,
    pub typ: TypeState,
}

impl<TypeState> Member<TypeState> {
    pub fn resolve(
        members: Vec<Self>,
        types: &mut BTreeMap<TypeId, Type>,
        local_types: &Vec<TypeId>,
    ) -> Result<Vec<Member<Typed>>, HayError> {
        let mut out = vec![];
        for m in members {
            let typ = Typed(TypeId::from_token(&m.token, types, local_types)?);
            out.push(Member {
                vis: m.vis,
                token: m.token,
                ident: m.ident,
                typ,
            })
        }

        Ok(out)
    }
}
