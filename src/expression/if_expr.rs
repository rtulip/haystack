use super::Expr;

#[derive(Debug, Clone)]
pub struct IfExpr<'src> {
    pub then: Box<Expr<'src>>,
    pub otherwise: Box<Expr<'src>>,
}

impl<'src> IfExpr<'src> {
    pub fn new<E1, E2>(then: E1, otherwise: E2) -> Self
    where
        E1: Into<Expr<'src>>,
        E2: Into<Expr<'src>>,
    {
        Self {
            then: Box::new(then.into()),
            otherwise: Box::new(otherwise.into()),
        }
    }
}
