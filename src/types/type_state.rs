use crate::types::TypeId;
/// Represents type information which is not yet known
#[derive(Debug, Clone, Copy)]
pub struct Untyped;

/// Represents collected type information
#[derive(Debug, Clone)]
pub struct Typed(pub TypeId);
