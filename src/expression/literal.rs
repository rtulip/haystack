use std::convert::From;

use crate::types::{FnTy, Ty};

#[derive(Debug, Clone)]
pub enum LiteralExpr<'a> {
    U32(u32),
    String(&'a str),
    Bool(bool),
}

impl<'a> LiteralExpr<'a> {}

impl<'a> From<&'a str> for LiteralExpr<'a> {
    fn from(value: &'a str) -> Self {
        Self::String(value)
    }
}

impl<'a> From<bool> for LiteralExpr<'a> {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl<'a> From<u32> for LiteralExpr<'a> {
    fn from(value: u32) -> Self {
        Self::U32(value)
    }
}
