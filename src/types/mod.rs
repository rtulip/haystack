mod base;
mod function;
mod interface;
mod pointer;
mod record;
mod substitution;
mod r#type;
mod type_var;
mod variant;

pub use base::*;
pub use function::*;
pub use interface::*;
pub use pointer::*;
pub use r#type::*;
pub use record::*;
pub use substitution::*;
pub use type_var::*;
pub use variant::*;

pub type Stack = Vec<Type>;
pub type Frame = Vec<(String, Type)>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(pub String);

impl TypeId {
    pub fn new<S: Into<String>>(s: S) -> Self {
        TypeId(s.into())
    }
}
