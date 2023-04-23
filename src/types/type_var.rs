use std::fmt::Debug;

#[derive(Clone, PartialEq, Eq)]
pub struct TypeVar(String);

impl Debug for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "∀{}", self.0)
    }
}
