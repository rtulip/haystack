use crate::ast::stmt::Stmt;
use crate::backend::{compile, Instruction, X86_64};
use crate::error::HayError;
use crate::lex::token::Loc;
use crate::types::{Type, TypeId, TypeMap};
use std::io::{self, Write};
use std::path::Path;
use std::process::{Command, Output};

pub mod test_tools;

pub fn compile_haystack(input_path: String, run: bool) -> Result<(), HayError> {
    let stmts = Stmt::from_file_with_prelude(&input_path)?;
    let (mut types, mut global_env, mut init_data, uninit_data) =
        Stmt::build_types_and_data(stmts)?;
    Type::type_check_functions(&mut types, &mut global_env)?;
    let fn_instructions = Instruction::from_type_map(&types, &mut init_data);
    check_for_entry_point(&types, &input_path)?;

    let path = Path::new(&input_path);
    compile::<X86_64>(
        path.with_extension("asm").to_str().unwrap(),
        &fn_instructions,
        &init_data,
        &uninit_data,
    )
    .unwrap();

    // assembler
    run_command(
        "nasm",
        vec!["-felf64", path.with_extension("asm").to_str().unwrap()],
        &input_path,
        false,
    )?;

    // linker
    run_command(
        "ld",
        vec![
            "-o",
            path.file_stem().unwrap().to_str().unwrap(),
            path.with_extension("o").to_str().unwrap(),
        ],
        &input_path,
        false,
    )?;

    if run {
        // run the exe
        run_command(
            format!("./{}", &path.file_stem().unwrap().to_str().unwrap()).as_str(),
            vec![],
            &input_path,
            true,
        )?;
    }

    Ok(())
}

pub fn run_command(
    cmd: &str,
    args: Vec<&str>,
    input_path: &String,
    verbose: bool,
) -> Result<Output, HayError> {
    print!("[CMD]: {cmd}");
    args.iter().for_each(|arg| print!(" {arg}"));
    println!();
    match Command::new(cmd).args(args).output() {
        Ok(output) => {
            if verbose {
                io::stdout().write_all(&output.stdout).unwrap();
                io::stderr().write_all(&output.stderr).unwrap();
            }
            Ok(output)
        }
        Err(e) => Err(
            HayError::new("Command Failed", Loc::new(input_path, 1, 1, 1))
                .with_hint(format!("Command: {cmd}"))
                .with_hint(format!("Error: {e:?}")),
        ),
    }
}

fn check_for_entry_point(types: &TypeMap, input_path: &String) -> Result<(), HayError> {
    if !types
        .iter()
        .any(|(tid, t)| matches!(t, Type::Function { .. }) && tid == &TypeId::new("main"))
    {
        Err(HayError::new(
            "No entry point was found",
            Loc::new(input_path, 1, 1, 1),
        ))
    } else {
        Ok(())
    }
}

mod functional {
    #[test]
    fn early_return_basic() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "early_return_basic", None)
    }

    #[test]
    fn early_return_if_else_while() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "early_return_if_else_while", None)
    }

    #[test]
    fn early_return_while_condition() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "early_return_while_condition", None)
    }

    #[test]
    fn array() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "array", None)
    }

    #[test]
    fn cat() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "cat", None)
    }

    #[test]
    fn hay_enum() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "enum", None)
    }

    #[test]
    fn generic_struct() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "generic_struct", None)
    }

    #[test]
    fn hello_world() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "hello_world", None)
    }

    #[test]
    fn if_else() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "if_else", None)
    }

    #[test]
    fn hay_impl() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "impl", None)
    }

    #[test]
    fn linear_map() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "linear_map", None)
    }

    #[test]
    fn local() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "local", None)
    }

    #[test]
    fn math() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "math", None)
    }

    #[test]
    fn nested_ident() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "nested_ident", None)
    }

    #[test]
    fn option() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "option", None)
    }

    #[test]
    fn pointer() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "pointer", None)
    }

    #[test]
    fn struct_accessors() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "struct_accessors", None)
    }

    #[test]
    fn hay_struct() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "struct", None)
    }

    #[test]
    fn hay_union() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "union", None)
    }

    #[test]
    fn address_of_framed() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "address_of_framed", None)
    }

    #[test]
    fn inner_address_of_framed() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "inner_address_of_framed", None)
    }

    #[test]
    fn inline_fn() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "inline_fn", None)?;

        let asm = std::fs::read_to_string("src/tests/functional/inline_fn.asm").unwrap();
        assert!(
            asm.find("call fn_my__add").is_none(),
            "Found a call to `fn_my__add` in the generated assembly!"
        );
        assert!(
            asm.find("fn_my__add:").is_some(),
            "Didn't find any definition of `fn_my__add` in generated assembly"
        );
        Ok(())
    }

    #[test]
    fn pointer_offsets() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "pointer_offsets", None)
    }

    #[test]
    fn generic_fn_with_const_ptr_arg() -> Result<(), std::io::Error> {
        super::test_tools::run_test(
            "src/tests/functional",
            "generic_fn_with_const_ptr_arg",
            None,
        )
    }

    #[test]
    fn valid_pointer_operations() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "valid_pointer_operations", None)
    }

    #[test]
    fn pointer_types() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "pointer_types", None)
    }

    #[test]
    fn mutable_fn_input() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "mutable_fn_input", None)
    }

    #[test]
    fn multiple_mutable_bindings() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "multiple_mutable_bindings", None)
    }

    #[test]
    fn inline_impl_fn() -> Result<(), std::io::Error> {
        use crate::backend::{CodeGen, X86_64};
        super::test_tools::run_test("src/tests/functional", "inline_impl_fn", None)?;

        let asm = std::fs::read_to_string("src/tests/functional/inline_impl_fn.asm").unwrap();
        let fn_name = X86_64::encode_name("Foo.add");
        assert!(
            asm.find(format!("call {fn_name}").as_str()).is_none(),
            "Found a call to `{fn_name}` in the generated assembly!"
        );
        assert!(
            asm.find(format!("{fn_name}:").as_str()).is_some(),
            "Didn't find any definition of `{fn_name}` in generated assembly"
        );
        Ok(())
    }

    #[test]
    fn address_of_union() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "address_of_union", None)
    }

    #[test]
    fn empty_string() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "empty_string", None)
    }

    #[test]
    fn vec() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "vec", None)
    }

    #[test]
    fn result() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "result", None)
    }

    #[test]
    fn hstring() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "hstring", None)
    }

    #[test]
    fn pre_declare() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "pre_declare", None)
    }

    #[test]
    fn interface() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "interface", None)
    }

    #[test]
    fn blanket_impl() -> Result<(), std::io::Error> {
        super::test_tools::run_test("src/tests/functional", "blanket_impl", None)
    }

    #[test]
    fn blanket_impl_override() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "blanket_impl_override", None)
    }

    #[test]
    fn aliasing() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "aliasing", None)
    }

    #[test]
    fn print_to_string_fmt() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "print_to_string_fmt", None)
    }

    #[test]
    fn vec_formatting() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "vec_formatting", None)
    }

    #[test]
    fn generic_pair_add() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "generic_pair_add", None)
    }

    #[test]
    fn tuple_printing() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "tuple_printing", None)
    }

    #[test]
    fn array_of_tuples() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "array_of_tuples", None)
    }

    #[test]
    fn tuple_expressions() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "tuple_expressions", None)
    }

    #[test]
    fn tuple_expressions2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "tuple_expressions2", None)
    }

    #[test]
    fn associated_type_in_return() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/functional",
            "associated_type_in_return",
            None,
        )
    }

    #[test]
    fn enum_variants() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "enum_variants", None)
    }

    #[test]
    fn enum_variants_branching() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/functional",
            "enum_variants_branching",
            None,
        )
    }

    #[test]
    fn enum_variants_loop() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "enum_variants_loop", None)
    }

    #[test]
    fn concrete_sum_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "concrete_sum_type", None)
    }

    #[test]
    fn match_on_variant() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "match_on_variant", None)
    }

    #[test]
    fn enum_struct_generic1() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "enum_struct_generic1", None)
    }

    #[test]
    fn enum_struct_generic2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "enum_struct_generic2", None)
    }

    #[test]
    fn enum_struct_generic_else() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/functional",
            "enum_struct_generic_else",
            None,
        )
    }

    #[test]
    fn enum_struct_match_else_only() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/functional",
            "enum_struct_match_else_only",
            None,
        )
    }

    #[test]
    fn enum_struct_match_nested() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/functional",
            "enum_struct_match_nested",
            None,
        )
    }

    #[test]
    fn zero_sized_type_pointer_ops() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/functional",
            "zero_sized_type_pointer_ops",
            None,
        )
    }

    #[test]
    fn tuple_destructure() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "tuple_destructure", None)
    }

    #[test]
    fn unpack_tuple() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "unpack_tuple", None)
    }

    #[test]
    fn anonymous_structures() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "anonymous_structures", None)
    }

    #[test]
    fn number_literal_bases() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "number_literal_bases", None)
    }

    #[test]
    fn tuple_accessors_unary() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/functional", "tuple_accessors_unary", None)
    }
}

mod examples {
    #[test]
    fn function() -> Result<(), std::io::Error> {
        super::test_tools::run_test("examples", "function", Some("src/tests/examples"))
    }

    #[test]
    fn generics() -> Result<(), std::io::Error> {
        super::test_tools::run_test("examples", "generics", Some("src/tests/examples"))
    }

    #[test]
    fn hello() -> Result<(), std::io::Error> {
        super::test_tools::run_test("examples", "hello", Some("src/tests/examples"))
    }

    #[test]
    fn interfaces() -> Result<(), std::io::Error> {
        super::test_tools::run_test("examples", "interfaces", Some("src/tests/examples"))
    }

    #[test]
    fn loops_and_branching() -> Result<(), std::io::Error> {
        super::test_tools::run_test(
            "examples",
            "loops_and_branching",
            Some("src/tests/examples"),
        )
    }

    #[test]
    fn numbers_1() -> Result<(), std::io::Error> {
        super::test_tools::run_test("examples", "numbers_1", Some("src/tests/examples"))
    }

    #[test]
    fn numbers_2() -> Result<(), std::io::Error> {
        super::test_tools::run_test("examples", "numbers_2", Some("src/tests/examples"))
    }

    #[test]
    fn pointers() -> Result<(), std::io::Error> {
        super::test_tools::run_test("examples", "pointers", Some("src/tests/examples"))
    }

    #[test]
    fn user_defined_types() -> Result<(), std::io::Error> {
        super::test_tools::run_test("examples", "user_defined_types", Some("src/tests/examples"))
    }

    #[test]
    fn variable() -> Result<(), std::io::Error> {
        super::test_tools::run_test("examples", "variable", Some("src/tests/examples"))
    }

    #[test]
    fn advanced_features() -> Result<(), std::io::Error> {
        super::test_tools::run_test("examples", "advanced_features", Some("src/tests/examples"))
    }
}
