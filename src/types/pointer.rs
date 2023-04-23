use super::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PointerType {
    pub mutable: bool,
    pub inner: Box<Type>,
}
