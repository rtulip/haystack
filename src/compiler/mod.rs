use crate::ast::parser::Parser;
use crate::ast::stmt::Stmt;
use crate::backend::Instruction;
use crate::error::HayError;
use crate::lex::scanner::Scanner;
use crate::lex::token::Loc;
use crate::types::{Type, TypeId};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::io::{self, Write};
use std::process::{Command, Output};

pub mod test_tools;

pub fn parse_haystack_into_statements(
    input_path: &String,
    visited: &mut HashSet<String>,
) -> Result<Vec<Stmt>, HayError> {
    if visited.contains(input_path) {
        return Ok(vec![]);
    }

    if let Ok(source) = std::fs::read_to_string(input_path) {
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
    while types
        .iter()
        .filter(|(_, v)| matches!(v, Type::UncheckedFunction { .. }))
        .count()
        != 0
    {
        let fns = types
            .drain_filter(|_, v| matches!(v, Type::UncheckedFunction { .. }))
            .collect::<Vec<(TypeId, Type)>>();

        for (tid, f) in fns {
            if let Type::UncheckedFunction {
                token,
                name,
                inputs,
                outputs,
                body,
                generic_map,
            } = f
            {
                let mut stack = vec![];
                let mut frame = vec![];

                inputs.iter().rev().for_each(|arg| {
                    if arg.ident.is_some() {
                        frame.push((
                            arg.ident.as_ref().unwrap().lexeme.clone(),
                            arg.typ.0.clone(),
                        ))
                    } else {
                        stack.push(arg.typ.0.clone())
                    }
                });

                let mut typed_body = vec![];
                for expr in body {
                    typed_body.push(expr.type_check(
                        &mut stack,
                        &mut frame,
                        &global_env,
                        &mut types,
                        &generic_map,
                    )?);
                }

                types.insert(
                    tid,
                    Type::Function {
                        token,
                        name,
                        inputs,
                        outputs,
                        body: typed_body,
                        generic_map,
                    },
                );
            }
        }
    }

    let mut ops = vec![];

    types
        .iter()
        .filter(|(_, t)| matches!(t, Type::Function { .. }))
        .for_each(|(tid, func)| {
            ops.push((tid, Instruction::from_function(func.clone(), &types)));
        });

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

    #[test]
    fn array() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/array")
    }

    #[test]
    fn auto_functions() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/auto_functions")
    }

    #[test]
    fn cat() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/cat")
    }

    #[test]
    fn r#enum() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/enum")
    }

    #[test]
    fn generic_struct() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/generic_struct")
    }

    #[test]
    fn hello_world() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/hello_world")
    }

    #[test]
    fn if_else() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/if_else")
    }

    #[test]
    fn r#impl() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/impl")
    }

    #[test]
    fn linear_map() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/linear_map")
    }

    #[test]
    fn local() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/local")
    }

    #[test]
    fn math() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/math")
    }

    #[test]
    fn nested_ident() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/nested_ident")
    }

    #[test]
    fn option() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/option")
    }

    #[test]
    fn pointer() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/pointer")
    }

    #[test]
    fn scoped_as() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/scoped_as")
    }

    #[test]
    fn stacks() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/stacks")
    }

    #[test]
    fn struct_accessors() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/struct_accessors")
    }

    #[test]
    fn r#struct() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/struct")
    }

    #[test]
    fn r#union() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional/union")
    }

    #[test]
    fn parse_as_block_bad_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_as_block_bad_close")
    }

    #[test]
    fn parse_as_block_bad_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_as_block_bad_open")
    }

    #[test]
    fn parse_bad_accessor1() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_accessor1")
    }

    #[test]
    fn parse_bad_accessor2() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_accessor2")
    }

    #[test]
    fn parse_bad_accessor3() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_accessor3")
    }

    #[test]
    fn parse_bad_annotated_call_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_annotated_call_close")
    }

    #[test]
    fn parse_bad_annotated_type() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_annotated_type")
    }

    #[test]
    fn parse_bad_arg_identifier() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_arg_identifier")
    }

    #[test]
    fn parse_bad_array_var_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_array_var_close")
    }

    #[test]
    fn parse_bad_array_var_size() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_array_var_size")
    }

    #[test]
    fn parse_bad_block_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_block_close")
    }

    #[test]
    fn parse_bad_block_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_block_open")
    }

    #[test]
    fn parse_bad_body_after_function_name() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_body_after_function_name")
    }

    #[test]
    fn parse_bad_cast_expr_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_cast_expr_close")
    }

    #[test]
    fn parse_bad_cast_expr_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_cast_expr_open")
    }

    #[test]
    fn parse_bad_cast_expr_param() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_cast_expr_param")
    }

    #[test]
    fn parse_bad_else_if_block() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_else_if_block")
    }

    #[test]
    fn parse_bad_expression() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_expression")
    }

    #[test]
    fn parse_bad_file_to_include() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_file_to_include")
    }

    #[test]
    fn parse_bad_function_annotation_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_function_annotation_close")
    }

    #[test]
    fn parse_bad_function_parameter_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_function_parameter_close")
    }

    #[test]
    fn parse_bad_function_return_list_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_function_return_list_close")
    }

    #[test]
    fn parse_bad_function_return_list_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_function_return_list_open")
    }

    #[test]
    fn parse_bad_include_statement() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_include_statement")
    }

    #[test]
    fn parse_bad_pointer_type() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_pointer_type")
    }

    #[test]
    fn parse_bad_top_level_token() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_top_level_token")
    }

    #[test]
    fn parse_enum_bad_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_enum_bad_close")
    }

    #[test]
    fn parse_enum_bad_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_enum_bad_open")
    }

    #[test]
    fn parse_enum_empty_variants() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_enum_empty_variants")
    }

    #[test]
    fn parse_enum_without_identifier() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_enum_without_identifier")
    }

    #[test]
    fn parse_function_empty_return_list() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_function_empty_return_list")
    }

    #[test]
    fn parse_function_without_name() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_function_without_name")
    }

    #[test]
    fn parse_mixed_identifier_arg_list() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_mixed_identifier_arg_list")
    }

    #[test]
    fn parse_no_args_in_annotated_call() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_no_args_in_annotated_call")
    }

    #[test]
    fn parse_struct_bad_annotations_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_struct_bad_annotations_close")
    }

    #[test]
    fn parse_struct_bad_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_struct_bad_close")
    }

    #[test]
    fn parse_struct_bad_impl_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_struct_bad_impl_open")
    }

    #[test]
    fn parse_struct_bad_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_struct_bad_open")
    }

    #[test]
    fn parse_struct_empty_members() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_struct_empty_members")
    }

    #[test]
    fn parse_struct_member_without_identifier1() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_struct_member_without_identifier1")
    }

    #[test]
    fn parse_struct_member_without_identifier2() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_struct_member_without_identifier2")
    }

    #[test]
    fn parse_struct_pub_member_without_type() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_struct_pub_member_without_type")
    }

    #[test]
    fn parse_struct_without_identifier() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_struct_without_identifier")
    }

    #[test]
    fn parse_var_expr_bad_ident() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_var_expr_bad_ident")
    }

    #[test]
    fn parse_var_expr_bad_type() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_var_expr_bad_type")
    }

    #[test]
    fn parse_var_expr_missing_colon() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_var_expr_missing_colon")
    }

    #[test]
    fn parse_bad_sizeof_open() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_sizeof_open")
    }

    #[test]
    fn parse_bad_sizeof_close() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_sizeof_close")
    }

    #[test]
    fn parse_bad_sizeof_param() -> Result<(), std::io::Error> {
        super::test_tools::run_test("parser/parse_bad_sizeof_param")
    }
}
