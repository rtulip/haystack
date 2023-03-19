mod framed_type;
mod function;
mod interface;
mod record_kind;
mod signature;
mod r#type;
mod type_id;
mod variance;
mod variant;

pub use framed_type::*;
pub use function::*;
pub use interface::*;
pub use r#type::*;
pub use record_kind::*;
pub use signature::*;
pub use type_id::*;
pub use variance::*;
pub use variant::*;

use std::collections::BTreeMap;
pub type TypeMap = BTreeMap<TypeId, Type>;
pub type Stack = Vec<TypeId>;
pub type Frame = Vec<(String, FramedType)>;

pub fn stack_compare(
    input: &Stack,
    stack: &Stack,
    types: &mut TypeMap,
    variance: Variance,
) -> bool {
    for (input, stk) in stack.iter().rev().zip(input.iter().rev()) {
        let v = Variance::new(input, stk, types);
        if v > variance {
            return false;
        }
    }
    true
}

pub fn stack_compare_exact(
    input: &Stack,
    stack: &Stack,
    types: &mut TypeMap,
    variance: Variance,
) -> bool {
    if stack.len() != input.len() {
        return false;
    }
    stack_compare(input, stack, types, variance)
}
mod tests {

    #[test]
    fn record_record_resolution() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "record_record_resolution",
            None,
        )
    }

    #[test]
    fn generic_record_record_resolution() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "generic_record_record_resolution",
            None,
        )
    }

    #[test]
    fn enum_enum_resolution() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "enum_enum_resolution", None)
    }

    #[test]
    fn generic_record_size() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "generic_record_size", None)
    }

    #[test]
    fn immutable_pointer_write() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "immutable_pointer_write",
            None,
        )
    }
}
