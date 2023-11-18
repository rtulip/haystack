use std::convert::From;

use crate::expression::{BlockExpr, Expr, LiteralExpr, VarExpr};

use super::{Interpreter, InterpreterError};

#[derive(Debug, Clone)]
pub enum Element<'src> {
    Bool(bool),
    U32(u32),
    Str(&'src str),
    Expr(Expr<'src>),
    Extern(fn(&mut Interpreter) -> Result<(), InterpreterError>),
}

impl<'src> Element<'src> {
    pub fn expr(&self) -> &Expr<'src> {
        match self {
            Element::Expr(e) => e,
            _ => panic!(),
        }
    }
}

impl<'src> From<LiteralExpr<'src>> for Element<'src> {
    fn from(value: LiteralExpr<'src>) -> Self {
        match value {
            LiteralExpr::U32(n) => Element::U32(n),
            LiteralExpr::String(s) => Element::Str(s),
            LiteralExpr::Bool(b) => Element::Bool(b),
        }
    }
}

impl<'src> From<Expr<'src>> for Element<'src> {
    fn from(value: Expr<'src>) -> Self {
        Self::Expr(value)
    }
}

impl<'src> From<bool> for Element<'src> {
    fn from(value: bool) -> Self {
        Element::Bool(value)
    }
}

impl<'src> From<u32> for Element<'src> {
    fn from(value: u32) -> Self {
        Element::U32(value)
    }
}

impl<'src> From<&'src str> for Element<'src> {
    fn from(value: &'src str) -> Self {
        Element::Str(value)
    }
}
