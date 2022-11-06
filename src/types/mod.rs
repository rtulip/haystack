mod type_info;
mod type_state;
use std::collections::BTreeMap;

pub use type_info::*;
pub use type_state::*;

pub type TypeMap = BTreeMap<TypeId, Type>;
