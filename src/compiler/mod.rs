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
    let (mut types, global_env, mut init_data, uninit_data) = Stmt::build_types_and_data(stmts)?;
    Type::type_check_functions(&mut types, &global_env)?;
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
                .with_hint(format!("{:?}", e)),
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

mod tests {

    #[test]
    fn array() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "array")
    }

    #[test]
    fn cat() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "cat")
    }

    #[test]
    fn r#enum() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "enum")
    }

    #[test]
    fn generic_struct() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "generic_struct")
    }

    #[test]
    fn hello_world() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "hello_world")
    }

    #[test]
    fn if_else() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "if_else")
    }

    #[test]
    fn r#impl() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "impl")
    }

    #[test]
    fn linear_map() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "linear_map")
    }

    #[test]
    fn local() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "local")
    }

    #[test]
    fn math() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "math")
    }

    #[test]
    fn nested_ident() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "nested_ident")
    }

    #[test]
    fn option() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "option")
    }

    #[test]
    fn pointer() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "pointer")
    }

    #[test]
    fn scoped_as() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "scoped_as")
    }

    #[test]
    fn stacks() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "stacks")
    }

    #[test]
    fn struct_accessors() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "struct_accessors")
    }

    #[test]
    fn r#struct() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "struct")
    }

    #[test]
    fn r#union() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "union")
    }

    #[test]
    fn parse_as_block_bad_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_as_block_bad_close")
    }

    #[test]
    fn parse_as_block_bad_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_as_block_bad_open")
    }

    #[test]
    fn parse_bad_accessor1() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_accessor1")
    }

    #[test]
    fn parse_bad_accessor2() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_accessor2")
    }

    #[test]
    fn parse_bad_accessor3() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_accessor3")
    }

    #[test]
    fn parse_bad_annotated_call_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_annotated_call_close")
    }

    #[test]
    fn parse_bad_annotated_type() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_annotated_type")
    }

    #[test]
    fn parse_bad_arg_identifier() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_arg_identifier")
    }

    #[test]
    fn parse_bad_array_var_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_array_var_close")
    }

    #[test]
    fn parse_bad_array_var_size() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_array_var_size")
    }

    #[test]
    fn parse_bad_block_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_block_close")
    }

    #[test]
    fn parse_bad_block_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_block_open")
    }

    #[test]
    fn parse_bad_body_after_function_name() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_body_after_function_name")
    }

    #[test]
    fn parse_bad_cast_expr_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_cast_expr_close")
    }

    #[test]
    fn parse_bad_cast_expr_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_cast_expr_open")
    }

    #[test]
    fn parse_bad_cast_expr_param() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_cast_expr_param")
    }

    #[test]
    fn parse_bad_else_if_block() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_else_if_block")
    }

    #[test]
    fn parse_bad_expression() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_expression")
    }

    #[test]
    fn parse_bad_file_to_include() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_file_to_include")
    }

    #[test]
    fn parse_bad_function_annotation_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_function_annotation_close")
    }

    #[test]
    fn parse_bad_function_parameter_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_function_parameter_close")
    }

    #[test]
    fn parse_bad_function_return_list_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_function_return_list_close")
    }

    #[test]
    fn parse_bad_function_return_list_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_function_return_list_open")
    }

    #[test]
    fn parse_bad_include_statement() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_include_statement")
    }

    #[test]
    fn parse_bad_pointer_type() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_pointer_type")
    }

    #[test]
    fn parse_bad_top_level_token() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_top_level_token")
    }

    #[test]
    fn parse_enum_bad_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_enum_bad_close")
    }

    #[test]
    fn parse_enum_bad_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_enum_bad_open")
    }

    #[test]
    fn parse_enum_empty_variants() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_enum_empty_variants")
    }

    #[test]
    fn parse_enum_without_identifier() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_enum_without_identifier")
    }

    #[test]
    fn parse_function_empty_return_list() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_function_empty_return_list")
    }

    #[test]
    fn parse_function_without_name() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_function_without_name")
    }

    #[test]
    fn parse_mixed_identifier_arg_list() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_mixed_identifier_arg_list")
    }

    #[test]
    fn parse_no_args_in_annotated_call() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_no_args_in_annotated_call")
    }

    #[test]
    fn parse_struct_bad_annotations_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_struct_bad_annotations_close")
    }

    #[test]
    fn parse_struct_bad_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_struct_bad_close")
    }

    #[test]
    fn parse_struct_bad_impl_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_struct_bad_impl_open")
    }

    #[test]
    fn parse_struct_bad_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_struct_bad_open")
    }

    #[test]
    fn parse_struct_empty_members() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_struct_empty_members")
    }

    #[test]
    fn parse_struct_member_without_identifier1() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_struct_member_without_identifier1")
    }

    #[test]
    fn parse_struct_member_without_identifier2() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_struct_member_without_identifier2")
    }

    #[test]
    fn parse_struct_pub_member_without_type() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_struct_pub_member_without_type")
    }

    #[test]
    fn parse_struct_without_identifier() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_struct_without_identifier")
    }

    #[test]
    fn parse_var_expr_bad_ident() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_var_expr_bad_ident")
    }

    #[test]
    fn parse_var_expr_bad_type() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_var_expr_bad_type")
    }

    #[test]
    fn parse_var_expr_missing_colon() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_var_expr_missing_colon")
    }

    #[test]
    fn parse_bad_sizeof_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_sizeof_open")
    }

    #[test]
    fn parse_bad_sizeof_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_sizeof_close")
    }

    #[test]
    fn parse_bad_sizeof_param() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser", "parse_bad_sizeof_param")
    }
}
