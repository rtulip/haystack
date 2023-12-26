#[derive(Debug, Clone)]
pub enum TyInstanceExpr<'src> {
    Enum { base: &'src str, variant: &'src str },
}
