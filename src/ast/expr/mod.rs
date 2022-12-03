mod accessor;
mod annotated_call;
mod r#as;
mod cast;
mod expr_typ;
mod ident;
mod r#if;
mod literal;
mod operator;
mod r#return;
mod size_of;
mod syscall;
mod unary;
mod var;
mod r#while;

pub use accessor::*;
pub use annotated_call::*;
pub use cast::*;
pub use expr_typ::*;
pub use ident::*;
pub use literal::*;
pub use operator::*;
pub use r#as::*;
pub use r#if::*;
pub use r#return::*;
pub use r#while::*;
pub use size_of::*;
pub use syscall::*;
pub use unary::*;
pub use var::*;

mod tests {

    #[test]
    fn bad_early_return() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "bad_early_return")
    }

    #[test]
    fn ops_after_return() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "ops_after_return")
    }

    #[test]
    fn if_no_else_modify_stack() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "if_no_else_modify_stack")
    }

    #[test]
    fn incorrect_fn_signature() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "incorrect_fn_signature")
    }

    #[test]
    fn bind_insufficient_elements() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "bind_insufficient_elements")
    }

    #[test]
    fn annotations_on_non_generic_function() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "annotations_on_non_generic_function")
    }

    #[test]
    fn enum_multiple_inner_access() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "enum_multiple_inner_access")
    }

    #[test]
    fn enum_unknown_variant() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "enum_unknown_variant")
    }

    #[test]
    fn non_record_accessor() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "non_record_accessor")
    }

    #[test]
    fn struct_accessor_without_member() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "struct_accessor_without_member")
    }

    #[test]
    fn union_accessor_without_member() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "union_accessor_without_member")
    }

    #[test]
    fn unknown_accessor_ident() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "unknown_accessor_ident")
    }

    #[test]
    fn unresolved_generics_in_annotated_call() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "unresolved_generics_in_annotated_call")
    }

    #[test]
    fn cast_u8() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "cast_u8")
    }

    #[test]
    fn cast_enum() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "cast_enum")
    }

    #[test]
    fn cast_generic_struct_instance() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "cast_generic_struct_instance")
    }

    #[test]
    fn unrecognized_ident() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "unrecognized_ident")
    }

    #[test]
    fn if_block_different_stacks() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "if_block_different_stacks")
    }

    #[test]
    fn enum_compare() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "enum_compare")
    }

    #[test]
    fn size_of_unknown_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "size_of_unknown_type")
    }
    #[test]
    fn size_of_unknown_type_generic() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "size_of_unknown_type_generic")
    }

    #[test]
    fn syscall_bad_number_of_args() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "syscall_bad_number_of_args")
    }

    #[test]
    fn syscall_wrong_sized_types() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "syscall_wrong_sized_types")
    }

    #[test]
    fn while_changes_frame() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "while_changes_frame")
    }

    #[test]
    fn while_changes_stack() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "while_changes_stack")
    }

    #[test]
    fn var_unknown_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "var_unknown_type")
    }

    #[test]
    fn address_of_unknown_ident() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "address_of_unknown_ident")
    }

    #[test]
    fn private_member_access() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "private_member_access")
    }

    #[test]
    fn private_member_access_in_impl() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "private_member_access_in_impl")
    }

    #[test]
    fn inner_address_of_no_member() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "inner_address_of_no_member")
    }

    #[test]
    fn inner_address_of_non_record_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "inner_address_of_non_record_type")
    }

    #[test]
    fn inner_address_of_private_member_in_impl() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "type_check",
            "inner_address_of_private_member_in_impl",
        )
    }

    #[test]
    fn inner_address_of_private_member() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "inner_address_of_private_member")
    }

    #[test]
    fn multiple_pointer_offsets() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "multiple_pointer_offsets")
    }

    #[test]
    fn mutable_pointer_to_immutable_framed() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "mutable_pointer_to_immutable_framed")
    }

    #[test]
    fn mutable_pointer_to_immutable_framed_inner() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "type_check",
            "mutable_pointer_to_immutable_framed_inner",
        )
    }

    #[test]
    fn mutable_pointer_to_immutable_fn_arg() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "mutable_pointer_to_immutable_fn_arg")
    }

    #[test]
    fn if_else_push_between_conditions() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "if_else_push_between_conditions")
    }

    #[test]
    fn while_loop_push_before_condition() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "while_loop_push_before_condition")
    }

    #[test]
    fn bad_interface_resolution() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "bad_interface_resolution")
    }

    #[test]
    fn interface_instance_concrete() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "interface_instance_concrete")
    }

    #[test]
    fn interface_instance_generic() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "interface_instance_generic")
    }
}
