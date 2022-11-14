use std::collections::HashMap;

use crate::{
    ast::{
        arg::Arg,
        expr::{Expr, TypedExpr},
    },
    lex::token::Token,
    types::TypeId,
};

use super::Typed;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum FnTag {
    Inline,
    OnCopy,
    OnDrop,
}

#[derive(Debug, Clone)]
pub struct UncheckedFunction {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<Arg<Typed>>,
    pub outputs: Vec<Arg<Typed>>,
    pub body: Vec<Expr>,
    pub generic_map: Option<HashMap<TypeId, TypeId>>,
    pub tags: Vec<FnTag>,
}

#[derive(Debug, Clone)]
pub struct GenericFunction {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<Arg<Typed>>,
    pub outputs: Vec<Arg<Typed>>,
    pub generics: Vec<TypeId>,
    pub body: Vec<Expr>,
    pub tags: Vec<FnTag>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<Arg<Typed>>,
    pub outputs: Vec<Arg<Typed>>,
    pub body: Vec<TypedExpr>,
    pub generic_map: Option<HashMap<TypeId, TypeId>>,
    pub tags: Vec<FnTag>,
}

impl Function {
    pub fn has_tag(&self, tag: FnTag) -> bool {
        self.tags.contains(&tag)
    }
}
