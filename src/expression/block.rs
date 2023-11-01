use super::Expr;
use std::convert::From;

#[derive(Debug, Clone)]
pub struct BlockExpr<'a>(Vec<Expr<'a>>);

impl<'a, const N: usize> From<[Expr<'a>; N]> for BlockExpr<'a> {
    fn from(value: [Expr<'a>; N]) -> Self {
        Self(Vec::from(value))
    }
}
