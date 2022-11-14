mod function;
mod type_info;
use std::collections::BTreeMap;

pub use function::*;
pub use type_info::*;

pub type TypeMap = BTreeMap<TypeId, Type>;
