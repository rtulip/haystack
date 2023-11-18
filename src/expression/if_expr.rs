use super::Expr;

#[derive(Debug, Clone)]
pub struct IfExpr<'src> {
    pub then: Box<Expr<'src>>,
    pub otherwise: Option<Box<Expr<'src>>>,
}

impl<'src> IfExpr<'src> {
    pub fn new_full<E1, E2>(then: E1, otherwise: E2) -> Self
    where
        E1: Into<Expr<'src>>,
        E2: Into<Expr<'src>>,
    {
        Self {
            then: Box::new(then.into()),
            otherwise: Some(Box::new(otherwise.into())),
        }
    }

    pub fn new<E1>(then: E1) -> Self
    where
        E1: Into<Expr<'src>>,
    {
        Self {
            then: Box::new(then.into()),
            otherwise: None,
        }
    }
}
