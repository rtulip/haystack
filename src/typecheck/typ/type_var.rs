#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeVar(pub usize);
impl From<usize> for TypeVar {
    fn from(value: usize) -> Self {
        TypeVar(value)
    }
}
impl Into<usize> for TypeVar {
    fn into(self) -> usize {
        self.0
    }
}
