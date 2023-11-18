use std::convert::From;

#[derive(Debug, Clone)]
pub struct AsExpr<'src>(pub Vec<&'src str>);

impl<'src> AsExpr<'src> {}

impl<'src, T> From<T> for AsExpr<'src>
where
    T: Into<Vec<&'src str>>,
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}
