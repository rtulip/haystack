use std::convert::From;

#[derive(Debug, Clone)]
pub struct VarExpr<'a>(&'a str);

impl<'a> From<&'a str> for VarExpr<'a> {
    fn from(value: &'a str) -> Self {
        Self(value)
    }
}
