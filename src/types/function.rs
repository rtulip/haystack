use std::collections::HashMap;

use crate::{
    ast::{
        arg::TypedArg,
        expr::{Expr, TypedExpr},
    },
    lex::token::Token,
    types::TypeId,
};

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
    pub inputs: Vec<TypedArg>,
    pub outputs: Vec<TypedArg>,
    pub body: Vec<Expr>,
    pub generic_map: Option<HashMap<TypeId, TypeId>>,
    pub tags: Vec<FnTag>,
    pub impl_on: Option<TypeId>,
}

#[derive(Debug, Clone)]
pub struct GenericFunction {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<TypedArg>,
    pub outputs: Vec<TypedArg>,
    pub generics: Vec<TypeId>,
    pub body: Vec<Expr>,
    pub tags: Vec<FnTag>,
    pub impl_on: Option<TypeId>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<TypedArg>,
    pub outputs: Vec<TypedArg>,
    pub body: Vec<TypedExpr>,
    pub generic_map: Option<HashMap<TypeId, TypeId>>,
    pub tags: Vec<FnTag>,
}

impl Function {
    pub fn has_tag(&self, tag: FnTag) -> bool {
        self.tags.contains(&tag)
    }
}
