mod base;
mod function;
mod pointer;
mod record;
mod r#type;
mod type_var;

pub use base::BaseType;
pub use function::FunctionType;
pub use pointer::*;
pub use r#type::Type;
pub use record::{RecordKind, RecordType};
pub use type_var::TypeVar;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeId(String);

impl TypeId {
    pub fn new<S: Into<String>>(s: S) -> Self {
        TypeId(s.into())
    }
}
