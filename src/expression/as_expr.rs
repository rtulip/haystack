use std::convert::From;

#[derive(Debug, Clone)]
pub struct AsExpr<'src>(pub Vec<&'src str>);

impl<'src> AsExpr<'src> {}

impl<'src, const N: usize> From<[&'src str; N]> for AsExpr<'src> {
    fn from(value: [&'src str; N]) -> Self {
        Self(value.into())
    }
}

impl<'src> From<&'src str> for AsExpr<'src> {
    fn from(value: &'src str) -> Self {
        Self(vec![value])
    }
}
