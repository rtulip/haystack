pub use self::{block::BlockExpr, literal::LiteralExpr, var::VarExpr};
use std::convert::From;

mod block;
mod literal;
mod var;

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Literal(LiteralExpr<'a>),
    Block(BlockExpr<'a>),
    Var(VarExpr<'a>),
}

impl<'a> From<BlockExpr<'a>> for Expr<'a> {
    fn from(value: BlockExpr<'a>) -> Self {
        Self::Block(value)
    }
}

impl<'a> From<LiteralExpr<'a>> for Expr<'a> {
    fn from(value: LiteralExpr<'a>) -> Self {
        Self::Literal(value)
    }
}

impl<'a> From<VarExpr<'a>> for Expr<'a> {
    fn from(value: VarExpr<'a>) -> Self {
        Self::Var(value)
    }
}
