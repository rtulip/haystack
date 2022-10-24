use crate::ast::parser::Parser;
use crate::ast::stmt::Stmt;
use crate::error::HayError;
use crate::lex::scanner::Scanner;
use crate::lex::token::Loc;
use crate::types::{Type, TypeId};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::io::{self, Write};
use std::process::{Command, Output};

pub fn parse_haystack_into_statements(
    input_path: &String,
    visited: &mut HashSet<String>,
) -> Result<Vec<Stmt>, HayError> {
    if visited.contains(input_path) {
        return Ok(vec![]);
    }

    if let Ok(source) = std::fs::read_to_string(&input_path) {
        visited.insert(input_path.clone());
        let scanner = Scanner::new(input_path, &source);
        let tokens = scanner.scan_tokens()?;
        let parser = Parser::new(tokens, visited);
        let stmts = parser.parse()?;

        Ok(stmts)
    } else {
        Err(HayError::new(
            format!("Failed to read from file: {input_path}"),
            Loc::new(input_path, 0, 0, 0),
        ))
    }
}

pub fn compile_haystack(
    input_path: String,
    _run: bool,
    _ir: bool,
    _simple: bool,
) -> Result<(), HayError> {
    let mut visited = HashSet::new();
    let mut stmts =
        parse_haystack_into_statements(&String::from("src/libs/prelude.hay"), &mut visited)?;
    stmts.append(&mut parse_haystack_into_statements(
        &input_path,
        &mut visited,
    )?);

    let mut types: BTreeMap<TypeId, Type> = BTreeMap::new();
    types.insert(TypeId::new("u64"), Type::U64);
    types.insert(TypeId::new("u8"), Type::U8);
    types.insert(TypeId::new("char"), Type::Char);
    types.insert(TypeId::new("bool"), Type::Bool);

    let mut global_env = HashMap::new();
    for s in stmts {
        s.add_to_global_scope(&mut types, &mut global_env)?;
    }

    let mut round = 1;
    while types
        .iter()
        .filter(|(_, v)| matches!(v, Type::UncheckedFunction { .. }))
        .count()
        != 0
    {
        println!("Round {round}");
        let fns = types
            .drain_filter(|_, v| matches!(v, Type::UncheckedFunction { .. }))
            .collect::<Vec<(TypeId, Type)>>();

        for (tid, f) in fns {
            if let Type::UncheckedFunction {
                inputs,
                body,
                generic_map,
                ..
            } = f
            {
                let mut stack = vec![];
                let mut frame = vec![];

                inputs.iter().for_each(|arg| {
                    if arg.ident.is_some() {
                        frame.push((
                            arg.ident.as_ref().unwrap().lexeme.clone(),
                            arg.typ.0.clone(),
                        ))
                    } else {
                        stack.push(arg.typ.0.clone())
                    }
                });

                println!("Checking {tid}");

                for expr in body {
                    expr.type_check(
                        &mut stack,
                        &mut frame,
                        &global_env,
                        &mut types,
                        &generic_map,
                    )?;
                }
            }
        }

        round += 1;
    }

    Ok(())
}

pub fn run_command(cmd: &str, args: Vec<&str>) -> Output {
    print!("[CMD]: {cmd}");
    args.iter().for_each(|arg| print!(" {arg}"));
    println!();
    let nasm_output = Command::new(cmd)
        .args(args)
        .output()
        .expect("Failed to run nasm");
    io::stdout().write_all(&nasm_output.stdout).unwrap();
    io::stderr().write_all(&nasm_output.stderr).unwrap();
    nasm_output
}

mod tests {

    use serde::{Deserialize, Serialize};
    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct OutputSummary {
        exit_code: i32,
        stdout: String,
        stderr: String,
    }

    #[allow(dead_code)]
    pub fn run_test(file_base: &str) -> Result<(), std::io::Error> {
        use crate::compiler::run_command;
        use std::process::Output;

        fn summarize_output(output: &Output) -> OutputSummary {
            let exit_code = output.status.code().unwrap();
            let stdout = String::from_utf8(output.stdout.clone()).unwrap();
            let stderr = String::from_utf8(output.stderr.clone()).unwrap();
            OutputSummary {
                exit_code,
                stdout,
                stderr,
            }
        }
        let file = format!("src/tests/{file_base}");

        let output = run_command(
            "cargo",
            vec!["r", "-q", "--", format!("{file}.hay").as_str()],
        );
        let compilation_summary = summarize_output(&output);
        let com_path = format!("{file}.try_com");

        if std::path::Path::new(&com_path).exists() {
            let prev_output: OutputSummary =
                serde_json::from_str(&std::fs::read_to_string(&com_path)?.as_str())?;
            assert_eq!(prev_output, compilation_summary);
        } else {
            std::fs::write(
                com_path,
                serde_json::to_string_pretty(&compilation_summary)?,
            )?;
        }

        if output.status.success() {
            let output = summarize_output(&run_command(format!("./{file_base}",).as_str(), vec![]));

            let run_path = format!("{file}.try_run");

            if std::path::Path::new(&run_path).exists() {
                let prev_output: OutputSummary =
                    serde_json::from_str(&std::fs::read_to_string(&run_path)?.as_str())?;
                assert_eq!(prev_output, output);
            } else {
                std::fs::write(run_path, serde_json::to_string_pretty(&output)?)?;
            }
        }

        Ok(())
    }

    #[test]
    fn array() -> Result<(), std::io::Error> {
        run_test("array")
    }

    #[test]
    fn auto_functions() -> Result<(), std::io::Error> {
        run_test("auto_functions")
    }

    #[test]
    fn cat() -> Result<(), std::io::Error> {
        run_test("cat")
    }

    #[test]
    fn r#enum() -> Result<(), std::io::Error> {
        run_test("enum")
    }

    #[test]
    fn generic_struct() -> Result<(), std::io::Error> {
        run_test("generic_struct")
    }

    #[test]
    fn hello_world() -> Result<(), std::io::Error> {
        run_test("hello_world")
    }

    #[test]
    fn if_else() -> Result<(), std::io::Error> {
        run_test("if_else")
    }

    #[test]
    fn r#impl() -> Result<(), std::io::Error> {
        run_test("impl")
    }

    #[test]
    fn linear_map() -> Result<(), std::io::Error> {
        run_test("linear_map")
    }

    #[test]
    fn local() -> Result<(), std::io::Error> {
        run_test("local")
    }

    #[test]
    fn math() -> Result<(), std::io::Error> {
        run_test("math")
    }

    #[test]
    fn nested_ident() -> Result<(), std::io::Error> {
        run_test("nested_ident")
    }

    #[test]
    fn option() -> Result<(), std::io::Error> {
        run_test("option")
    }

    #[test]
    fn pointer() -> Result<(), std::io::Error> {
        run_test("pointer")
    }

    #[test]
    fn scoped_as() -> Result<(), std::io::Error> {
        run_test("scoped_as")
    }

    #[test]
    fn stacks() -> Result<(), std::io::Error> {
        run_test("stacks")
    }

    #[test]
    fn struct_accessors() -> Result<(), std::io::Error> {
        run_test("struct_accessors")
    }

    #[test]
    fn r#struct() -> Result<(), std::io::Error> {
        run_test("struct")
    }

    #[test]
    fn r#union() -> Result<(), std::io::Error> {
        run_test("union")
    }

    #[test]
    fn scan_unexpected_char() -> Result<(), std::io::Error> {
        run_test("scan_unexpected_char")
    }

    #[test]
    fn scan_bad_number_literal() -> Result<(), std::io::Error> {
        run_test("scan_bad_number_literal")
    }

    #[test]
    fn scan_bad_u8() -> Result<(), std::io::Error> {
        run_test("scan_bad_u8")
    }

    #[test]
    fn scan_unterminated_char() -> Result<(), std::io::Error> {
        run_test("scan_unterminated_char")
    }

    #[test]
    fn scan_unterminated_string() -> Result<(), std::io::Error> {
        run_test("scan_unterminated_string")
    }

    #[test]
    fn scan_bad_escaped_char() -> Result<(), std::io::Error> {
        run_test("scan_bad_escaped_char")
    }

    #[test]
    fn parse_as_block_bad_close() -> Result<(), std::io::Error> {
        run_test("parse_as_block_bad_close")
    }

    #[test]
    fn parse_as_block_bad_open() -> Result<(), std::io::Error> {
        run_test("parse_as_block_bad_open")
    }

    #[test]
    fn parse_bad_accessor1() -> Result<(), std::io::Error> {
        run_test("parse_bad_accessor1")
    }

    #[test]
    fn parse_bad_accessor2() -> Result<(), std::io::Error> {
        run_test("parse_bad_accessor2")
    }

    #[test]
    fn parse_bad_accessor3() -> Result<(), std::io::Error> {
        run_test("parse_bad_accessor3")
    }

    #[test]
    fn parse_bad_annotated_call_close() -> Result<(), std::io::Error> {
        run_test("parse_bad_annotated_call_close")
    }

    #[test]
    fn parse_bad_annotated_type() -> Result<(), std::io::Error> {
        run_test("parse_bad_annotated_type")
    }

    #[test]
    fn parse_bad_arg_identifier() -> Result<(), std::io::Error> {
        run_test("parse_bad_arg_identifier")
    }

    #[test]
    fn parse_bad_array_var_close() -> Result<(), std::io::Error> {
        run_test("parse_bad_array_var_close")
    }

    #[test]
    fn parse_bad_array_var_size() -> Result<(), std::io::Error> {
        run_test("parse_bad_array_var_size")
    }

    #[test]
    fn parse_bad_block_close() -> Result<(), std::io::Error> {
        run_test("parse_bad_block_close")
    }

    #[test]
    fn parse_bad_block_open() -> Result<(), std::io::Error> {
        run_test("parse_bad_block_open")
    }

    #[test]
    fn parse_bad_body_after_function_name() -> Result<(), std::io::Error> {
        run_test("parse_bad_body_after_function_name")
    }

    #[test]
    fn parse_bad_cast_expr_close() -> Result<(), std::io::Error> {
        run_test("parse_bad_cast_expr_close")
    }

    #[test]
    fn parse_bad_cast_expr_open() -> Result<(), std::io::Error> {
        run_test("parse_bad_cast_expr_open")
    }

    #[test]
    fn parse_bad_cast_expr_param() -> Result<(), std::io::Error> {
        run_test("parse_bad_cast_expr_param")
    }

    #[test]
    fn parse_bad_else_if_block() -> Result<(), std::io::Error> {
        run_test("parse_bad_else_if_block")
    }

    #[test]
    fn parse_bad_expression() -> Result<(), std::io::Error> {
        run_test("parse_bad_expression")
    }

    #[test]
    fn parse_bad_file_to_include() -> Result<(), std::io::Error> {
        run_test("parse_bad_file_to_include")
    }

    #[test]
    fn parse_bad_function_annotation_close() -> Result<(), std::io::Error> {
        run_test("parse_bad_function_annotation_close")
    }

    #[test]
    fn parse_bad_function_parameter_close() -> Result<(), std::io::Error> {
        run_test("parse_bad_function_parameter_close")
    }

    #[test]
    fn parse_bad_function_return_list_close() -> Result<(), std::io::Error> {
        run_test("parse_bad_function_return_list_close")
    }

    #[test]
    fn parse_bad_function_return_list_open() -> Result<(), std::io::Error> {
        run_test("parse_bad_function_return_list_open")
    }

    #[test]
    fn parse_bad_include_statement() -> Result<(), std::io::Error> {
        run_test("parse_bad_include_statement")
    }

    #[test]
    fn parse_bad_pointer_type() -> Result<(), std::io::Error> {
        run_test("parse_bad_pointer_type")
    }

    #[test]
    fn parse_bad_top_level_token() -> Result<(), std::io::Error> {
        run_test("parse_bad_top_level_token")
    }

    #[test]
    fn parse_enum_bad_close() -> Result<(), std::io::Error> {
        run_test("parse_enum_bad_close")
    }

    #[test]
    fn parse_enum_bad_open() -> Result<(), std::io::Error> {
        run_test("parse_enum_bad_open")
    }

    #[test]
    fn parse_enum_empty_variants() -> Result<(), std::io::Error> {
        run_test("parse_enum_empty_variants")
    }

    #[test]
    fn parse_enum_without_identifier() -> Result<(), std::io::Error> {
        run_test("parse_enum_without_identifier")
    }

    #[test]
    fn parse_function_empty_return_list() -> Result<(), std::io::Error> {
        run_test("parse_function_empty_return_list")
    }

    #[test]
    fn parse_function_without_name() -> Result<(), std::io::Error> {
        run_test("parse_function_without_name")
    }

    #[test]
    fn parse_mixed_identifier_arg_list() -> Result<(), std::io::Error> {
        run_test("parse_mixed_identifier_arg_list")
    }

    #[test]
    fn parse_no_args_in_annotated_call() -> Result<(), std::io::Error> {
        run_test("parse_no_args_in_annotated_call")
    }

    #[test]
    fn parse_struct_bad_annotations_close() -> Result<(), std::io::Error> {
        run_test("parse_struct_bad_annotations_close")
    }

    #[test]
    fn parse_struct_bad_close() -> Result<(), std::io::Error> {
        run_test("parse_struct_bad_close")
    }

    #[test]
    fn parse_struct_bad_impl_open() -> Result<(), std::io::Error> {
        run_test("parse_struct_bad_impl_open")
    }

    #[test]
    fn parse_struct_bad_open() -> Result<(), std::io::Error> {
        run_test("parse_struct_bad_open")
    }

    #[test]
    fn parse_struct_empty_members() -> Result<(), std::io::Error> {
        run_test("parse_struct_empty_members")
    }

    #[test]
    fn parse_struct_member_without_identifier1() -> Result<(), std::io::Error> {
        run_test("parse_struct_member_without_identifier1")
    }

    #[test]
    fn parse_struct_member_without_identifier2() -> Result<(), std::io::Error> {
        run_test("parse_struct_member_without_identifier2")
    }

    #[test]
    fn parse_struct_pub_member_without_type() -> Result<(), std::io::Error> {
        run_test("parse_struct_pub_member_without_type")
    }

    #[test]
    fn parse_struct_without_identifier() -> Result<(), std::io::Error> {
        run_test("parse_struct_without_identifier")
    }

    #[test]
    fn parse_var_expr_bad_ident() -> Result<(), std::io::Error> {
        run_test("parse_var_expr_bad_ident")
    }

    #[test]
    fn parse_var_expr_bad_type() -> Result<(), std::io::Error> {
        run_test("parse_var_expr_bad_type")
    }

    #[test]
    fn parse_var_expr_missing_colon() -> Result<(), std::io::Error> {
        run_test("parse_var_expr_missing_colon")
    }

    #[test]
    fn parse_bad_sizeof_open() -> Result<(), std::io::Error> {
        run_test("parse_bad_sizeof_open")
    }

    #[test]
    fn parse_bad_sizeof_close() -> Result<(), std::io::Error> {
        run_test("parse_bad_sizeof_close")
    }

    #[test]
    fn parse_bad_sizeof_param() -> Result<(), std::io::Error> {
        run_test("parse_bad_sizeof_param")
    }
}
