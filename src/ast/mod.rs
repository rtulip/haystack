pub mod arg;
pub mod expr;
pub mod member;
pub mod parser;
pub mod stmt;
pub mod visibility;

mod tests {

    #[test]
    fn fn_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "fn_name_conflict", None)
    }

    #[test]
    fn enum_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "enum_name_conflict", None)
    }

    #[test]
    fn record_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "record_name_conflict", None)
    }

    #[test]
    fn var_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "var_name_conflict", None)
    }

    #[test]
    fn pre_declare_generics_mismatch() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "pre_declare_generics_mismatch",
            None,
        )
    }

    #[test]
    fn pre_declare_generics_mismatch2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "pre_declare_generics_mismatch2",
            None,
        )
    }

    #[test]
    fn pre_declare_generics_mismatch3() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "pre_declare_generics_mismatch3",
            None,
        )
    }

    #[test]
    fn pre_declare_generics_mismatch4() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "pre_declare_generics_mismatch4",
            None,
        )
    }

    #[test]
    fn pre_declare_kind_mismatch() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "pre_declare_kind_mismatch", None)
    }

    #[test]
    fn pre_declare_kind_mismatch2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "pre_declare_kind_mismatch2", None)
    }

    #[test]
    fn dangling_pre_declaration() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "dangling_pre_declaration", None)
    }

    #[test]
    fn pre_decl_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "pre_decl_name_conflict", None)
    }

    #[test]
    fn associated_type_redefinition() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "associated_type_redefinition",
            None,
        )
    }

    #[test]
    fn bad_associated_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "bad_associated_type", None)
    }

    #[test]
    fn impl_non_interface_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "impl_non_interface_type", None)
    }

    #[test]
    fn impl_unknown_interface() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "impl_unknown_interface", None)
    }

    #[test]
    fn impl_wrong_annotations() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "impl_wrong_annotations", None)
    }

    #[test]
    fn interface_signature_mismatch() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "interface_signature_mismatch",
            None,
        )
    }

    #[test]
    fn missing_associated_types() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "missing_associated_types", None)
    }

    #[test]
    fn missing_interface_impls() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "missing_interface_impls", None)
    }

    #[test]
    fn unexpected_interface_function() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "unexpected_interface_function",
            None,
        )
    }

    #[test]
    fn interface_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "interface_name_conflict", None)
    }

    #[test]
    fn interface_requirements() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "interface_requirements", None)
    }

    #[test]
    fn funcion_non_interface_requirement1() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "funcion_non_interface_requirement1",
            None,
        )
    }

    #[test]
    fn funcion_non_interface_requirement2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "funcion_non_interface_requirement2",
            None,
        )
    }

    #[test]
    fn interface_non_interface_requirement1() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "interface_non_interface_requirement1",
            None,
        )
    }

    #[test]
    fn interface_non_interface_requirement2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "interface_non_interface_requirement2",
            None,
        )
    }

    #[test]
    fn record_non_interface_requirement1() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "record_non_interface_requirement1",
            None,
        )
    }

    #[test]
    fn record_non_interface_requirement2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "record_non_interface_requirement2",
            None,
        )
    }

    #[test]
    fn non_generic_function_requirements() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "non_generic_function_requirements",
            None,
        )
    }

    #[test]
    fn non_generic_record_requirements() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "non_generic_record_requirements",
            None,
        )
    }

    #[test]
    fn blanket_impl_requirement_breach() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/stmt",
            "blanket_impl_requirement_breach",
            None,
        )
    }

    #[test]
    fn interface_rimpl() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/stmt", "interface_reimpl", None)
    }
}
