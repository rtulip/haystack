mod base;
mod function;
mod pointer;
mod record;
mod r#type;
mod type_var;

pub use base::*;
pub use function::*;
pub use pointer::*;
pub use r#type::*;
pub use record::*;
pub use type_var::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(String);

impl TypeId {
    pub fn new<S: Into<String>>(s: S) -> Self {
        TypeId(s.into())
    }
}
