use crate::expr::Expr;

pub struct Function<'src, M, E> {
    name: &'src str,
    body: Expr<'src, M, E>,
}

impl<'src, M, E> Function<'src, M, E> {
    pub fn new(name: &'src str, body: Expr<'src, M, E>) -> Self {
        Function { name, body }
    }
}
