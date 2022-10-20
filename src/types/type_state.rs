use crate::types::TypeId;
#[derive(Debug, Clone, Copy)]
pub struct Untyped;

#[derive(Debug, Clone)]
pub struct ConcreteType(pub TypeId);

#[derive(Debug, Clone)]
pub struct Typed;
