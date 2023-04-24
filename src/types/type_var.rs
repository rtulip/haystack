use std::fmt::Debug;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeVar(String);

impl TypeVar {
    pub fn new<S: Into<String>>(s: S) -> Self {
        Self(s.into())
    }
}

impl Debug for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "∀{}", self.0)
    }
}
