mod framed_type;
mod function;
mod record_kind;
mod signature;
mod r#type;
mod type_id;

pub use framed_type::*;
pub use function::*;
pub use r#type::*;
pub use record_kind::*;
pub use signature::*;
use std::collections::BTreeMap;
pub use type_id::*;

pub type TypeMap = BTreeMap<TypeId, Type>;
pub type Stack = Vec<TypeId>;
pub type Frame = Vec<(String, FramedType)>;

mod tests {

    #[test]
    fn record_record_resolution() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "record_record_resolution")
    }

    #[test]
    fn generic_record_record_resolution() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "generic_record_record_resolution")
    }

    #[test]
    fn enum_enum_resolution() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "enum_enum_resolution")
    }

    #[test]
    fn generic_record_size() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "generic_record_size")
    }

    #[test]
    fn immutable_pointer_write() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "immutable_pointer_write")
    }
}
