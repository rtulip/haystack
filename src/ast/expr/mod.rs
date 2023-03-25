mod accessor;
mod annotated_call;
mod r#as;
mod block;
mod cast;
mod expr_typ;
mod ident;
mod r#if;
mod literal;
mod r#match;
mod never;
mod operator;
mod r#return;
mod size_of;
mod syscall;
mod tuple;
mod unary;
mod unpack;
mod var;
mod r#while;

pub use accessor::*;
pub use annotated_call::*;
pub use block::*;
pub use cast::*;
pub use expr_typ::*;
pub use ident::*;
pub use literal::*;
pub use never::*;
pub use operator::*;
pub use r#as::*;
pub use r#if::*;
pub use r#match::*;
pub use r#return::*;
pub use r#while::*;
pub use size_of::*;
pub use syscall::*;
pub use tuple::*;
pub use unary::*;
pub use unpack::*;
pub use var::*;

mod tests {

    #[test]
    fn bad_early_return() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "bad_early_return", None)
    }

    #[test]
    fn ops_after_return() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "ops_after_return", None)
    }

    #[test]
    fn if_no_else_modify_stack() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "if_no_else_modify_stack",
            None,
        )
    }

    #[test]
    fn incorrect_fn_signature() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "incorrect_fn_signature",
            None,
        )
    }

    #[test]
    fn bind_insufficient_elements() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "bind_insufficient_elements",
            None,
        )
    }

    #[test]
    fn annotations_on_non_generic_function() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "annotations_on_non_generic_function",
            None,
        )
    }

    #[test]
    fn enum_multiple_inner_access() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "enum_multiple_inner_access",
            None,
        )
    }

    #[test]
    fn enum_unknown_variant() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "enum_unknown_variant", None)
    }

    #[test]
    fn non_record_accessor() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "non_record_accessor", None)
    }

    #[test]
    fn struct_accessor_without_member() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "struct_accessor_without_member",
            None,
        )
    }

    #[test]
    fn union_accessor_without_member() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "union_accessor_without_member",
            None,
        )
    }

    #[test]
    fn unknown_accessor_ident() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "unknown_accessor_ident",
            None,
        )
    }

    #[test]
    fn unresolved_generics_in_annotated_call() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "unresolved_generics_in_annotated_call",
            None,
        )
    }

    #[test]
    fn cast_u8() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "cast_u8", None)
    }

    #[test]
    fn cast_enum() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "cast_enum", None)
    }

    #[test]
    fn cast_generic_struct_instance() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "cast_generic_struct_instance",
            None,
        )
    }

    #[test]
    fn unrecognized_ident() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "unrecognized_ident", None)
    }

    #[test]
    fn if_block_different_stacks() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "if_block_different_stacks",
            None,
        )
    }

    #[test]
    fn enum_compare() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "enum_compare", None)
    }

    #[test]
    fn size_of_unknown_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "size_of_unknown_type", None)
    }
    #[test]
    fn size_of_unknown_type_generic() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "size_of_unknown_type_generic",
            None,
        )
    }

    #[test]
    fn syscall_bad_number_of_args() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "syscall_bad_number_of_args",
            None,
        )
    }

    #[test]
    fn syscall_wrong_sized_types() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "syscall_wrong_sized_types",
            None,
        )
    }

    #[test]
    fn while_changes_frame() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "while_changes_frame", None)
    }

    #[test]
    fn while_changes_stack() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "while_changes_stack", None)
    }

    #[test]
    fn var_unknown_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "var_unknown_type", None)
    }

    #[test]
    fn address_of_unknown_ident() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "address_of_unknown_ident",
            None,
        )
    }

    #[test]
    fn private_member_access() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "private_member_access", None)
    }

    #[test]
    fn private_member_access_in_impl() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "private_member_access_in_impl",
            None,
        )
    }

    #[test]
    fn inner_address_of_no_member() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "inner_address_of_no_member",
            None,
        )
    }

    #[test]
    fn inner_address_of_non_record_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "inner_address_of_non_record_type",
            None,
        )
    }

    #[test]
    fn inner_address_of_private_member_in_impl() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "inner_address_of_private_member_in_impl",
            None,
        )
    }

    #[test]
    fn inner_address_of_private_member() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "inner_address_of_private_member",
            None,
        )
    }

    #[test]
    fn multiple_pointer_offsets() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "multiple_pointer_offsets",
            None,
        )
    }

    #[test]
    fn mutable_pointer_to_immutable_framed() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "mutable_pointer_to_immutable_framed",
            None,
        )
    }

    #[test]
    fn mutable_pointer_to_immutable_framed_inner() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "mutable_pointer_to_immutable_framed_inner",
            None,
        )
    }

    #[test]
    fn mutable_pointer_to_immutable_fn_arg() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "mutable_pointer_to_immutable_fn_arg",
            None,
        )
    }

    #[test]
    fn if_else_push_between_conditions() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "if_else_push_between_conditions",
            None,
        )
    }

    #[test]
    fn while_loop_push_before_condition() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "while_loop_push_before_condition",
            None,
        )
    }

    #[test]
    fn bad_interface_resolution() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "bad_interface_resolution",
            None,
        )
    }

    #[test]
    fn interface_instance_concrete() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "interface_instance_concrete",
            None,
        )
    }

    #[test]
    fn interface_instance_generic() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "interface_instance_generic",
            None,
        )
    }

    #[test]
    fn function_requirements() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "function_requirements", None)
    }

    #[test]
    fn struct_requirements() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "struct_requirements", None)
    }

    #[test]
    fn union_requirements() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "union_requirements", None)
    }

    #[test]
    fn unknown_associated_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "unknown_associated_type",
            None,
        )
    }

    #[test]
    fn cast_to_generic_union() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "cast_to_generic_union", None)
    }

    #[test]
    fn annotated_unknown_function_call() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "annotated_unknown_function_call",
            None,
        )
    }

    #[test]
    fn associated_types_no_impl() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "associated_types_no_impl",
            None,
        )
    }
    #[test]
    fn associated_types() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "associated_types", None)
    }
    #[test]
    fn associated_type_unknown_interface() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "associated_type_unknown_interface",
            None,
        )
    }
    #[test]
    fn enum_bad_variant() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "enum_bad_variant", None)
    }
    #[test]
    fn enum_bad_variant2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "enum_bad_variant2", None)
    }

    #[test]
    fn variant_bad_resolve() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "variant_bad_resolve", None)
    }

    #[test]
    fn variant_resolve_bad_variant() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "variant_resolve_bad_variant",
            None,
        )
    }

    #[test]
    fn variant_resolve_bad_bases() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "variant_resolve_bad_bases",
            None,
        )
    }

    #[test]
    fn cast_variant() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "cast_variant", None)
    }

    #[test]
    fn pointer_supertype_resolution() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "pointer_supertype_resolution",
            None,
        )
    }

    #[test]
    fn match_bad_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "match_bad_type", None)
    }

    #[test]
    fn match_empty_stack() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "match_empty_stack", None)
    }

    #[test]
    fn match_mismatched_bases() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "match_mismatched_bases",
            None,
        )
    }

    #[test]
    fn match_non_variant_case() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "match_non_variant_case",
            None,
        )
    }

    #[test]
    fn match_unknown_type_case() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "match_unknown_type_case",
            None,
        )
    }

    #[test]
    fn enum_struct_multiple_inner() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "enum_struct_multiple_inner",
            None,
        )
    }

    #[test]
    fn enum_struct_unknown_variant() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "enum_struct_unknown_variant",
            None,
        )
    }

    #[test]
    fn enum_struct_require_cast() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "enum_struct_require_cast",
            None,
        )
    }

    #[test]
    fn enum_struct_require_generics() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "enum_struct_require_generics",
            None,
        )
    }

    #[test]
    fn cast_enum_struct_generic() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "cast_enum_struct_generic",
            None,
        )
    }

    #[test]
    fn cast_enum_struct() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "cast_enum_struct", None)
    }

    #[test]
    fn match_non_exhaustive() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "match_non_exhaustive", None)
    }

    #[test]
    fn match_else_case() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "match_else_case", None)
    }

    #[test]
    fn match_empty() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "match_empty", None)
    }

    #[test]
    fn enum_struct_generic_base_fn_sig() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "enum_struct_generic_base_fn_sig",
            None,
        )
    }

    #[test]
    fn never_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "never_type", None)
    }

    #[test]
    fn never_type_input() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "never_type_input", None)
    }

    #[test]
    fn destructure_wrong_number_idents() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "destructure_wrong_number_idents",
            None,
        )
    }

    #[test]
    fn destructure_non_tuple() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "destructure_non_tuple", None)
    }

    #[test]
    fn unpack_non_tuple() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "unpack_non_tuple", None)
    }

    #[test]
    fn unpack_empty_stack() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/type_check", "unpack_empty_stack", None)
    }

    #[test]
    fn anon_struct_bad_accessor1() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "anon_struct_bad_accessor1",
            None,
        )
    }

    #[test]
    fn anon_struct_bad_accessor2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "anon_struct_bad_accessor2",
            None,
        )
    }

    #[test]
    fn cast_to_generic_struct_with_private_members_in_impl() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "cast_to_generic_struct_with_private_members_in_impl",
            None,
        )
    }

    #[test]
    fn cast_to_generic_struct_with_private_members() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "cast_to_generic_struct_with_private_members",
            None,
        )
    }

    #[test]
    fn cast_to_struct_with_private_members_in_impl() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "cast_to_struct_with_private_members_in_impl",
            None,
        )
    }

    #[test]
    fn cast_to_struct_with_private_members() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/type_check",
            "cast_to_struct_with_private_members",
            None,
        )
    }
}
