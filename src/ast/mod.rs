pub mod arg;
pub mod expr;
pub mod member;
pub mod parser;
pub mod stmt;
pub mod visibility;

mod tests {

    #[test]
    fn fn_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "fn_name_conflict")
    }

    #[test]
    fn enum_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "enum_name_conflict")
    }

    #[test]
    fn record_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "record_name_conflict")
    }

    #[test]
    fn var_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "var_name_conflict")
    }

    #[test]
    fn pre_declare_generics_mismatch() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "pre_declare_generics_mismatch")
    }

    #[test]
    fn pre_declare_generics_mismatch2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "pre_declare_generics_mismatch2")
    }

    #[test]
    fn pre_declare_generics_mismatch3() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "pre_declare_generics_mismatch3")
    }

    #[test]
    fn pre_declare_generics_mismatch4() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "pre_declare_generics_mismatch4")
    }

    #[test]
    fn pre_declare_kind_mismatch() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "pre_declare_kind_mismatch")
    }

    #[test]
    fn pre_declare_kind_mismatch2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "pre_declare_kind_mismatch2")
    }

    #[test]
    fn dangling_pre_declaration() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "dangling_pre_declaration")
    }

    #[test]
    fn pre_decl_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "pre_decl_name_conflict")
    }

    #[test]
    fn associated_type_redefinition() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "associated_type_redefinition")
    }

    #[test]
    fn bad_associated_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "bad_associated_type")
    }

    #[test]
    fn impl_non_interface_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "impl_non_interface_type")
    }

    #[test]
    fn impl_unknown_interface() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "impl_unknown_interface")
    }

    #[test]
    fn impl_wrong_annotations() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "impl_wrong_annotations")
    }

    #[test]
    fn interface_signature_mismatch() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "interface_signature_mismatch")
    }

    #[test]
    fn missing_associated_types() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "missing_associated_types")
    }

    #[test]
    fn missing_interface_impls() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "missing_interface_impls")
    }

    #[test]
    fn unexpected_interface_function() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "unexpected_interface_function")
    }

    #[test]
    fn interface_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "interface_name_conflict")
    }

    #[test]
    fn interface_requirements() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "interface_requirements")
    }

    #[test]
    fn funcion_non_interface_requirement1() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "funcion_non_interface_requirement1")
    }

    #[test]
    fn funcion_non_interface_requirement2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "funcion_non_interface_requirement2")
    }

    #[test]
    fn interface_non_interface_requirement1() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "interface_non_interface_requirement1")
    }

    #[test]
    fn interface_non_interface_requirement2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "interface_non_interface_requirement2")
    }

    #[test]
    fn record_non_interface_requirement1() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "record_non_interface_requirement1")
    }

    #[test]
    fn record_non_interface_requirement2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "record_non_interface_requirement2")
    }

    #[test]
    fn non_generic_function_requirements() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "non_generic_function_requirements")
    }

    #[test]
    fn non_generic_record_requirements() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "non_generic_record_requirements")
    }
}
