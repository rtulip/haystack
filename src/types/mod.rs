mod framed_type;
mod function;
mod type_info;
use std::collections::BTreeMap;

pub use framed_type::*;
pub use function::*;
pub use type_info::*;

pub type TypeMap = BTreeMap<TypeId, Type>;
