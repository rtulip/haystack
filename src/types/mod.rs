mod framed_type;
mod function;
mod record_kind;
mod type_id;
mod type_info;
use std::collections::BTreeMap;

pub use framed_type::*;
pub use function::*;
pub use record_kind::*;
pub use type_id::*;
pub use type_info::*;

pub type TypeMap = BTreeMap<TypeId, Type>;
