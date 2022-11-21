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
    fn early_return_basic() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "early_return_basic")
    }

    #[test]
    fn early_return_if_else_while() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "early_return_if_else_while")
    }

    #[test]
    fn early_return_while_condition() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "early_return_while_condition")
    }

    #[test]
    fn array() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "array")
    }

    #[test]
    fn cat() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "cat")
    }

    #[test]
    fn hay_enum() -> Result<(), std::io::Error> {
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
    fn hay_impl() -> Result<(), std::io::Error> {
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
    fn hay_struct() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "struct")
    }

    #[test]
    fn hay_union() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "union")
    }

    #[test]
    fn address_of_framed() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "address_of_framed")
    }

    #[test]
    fn inner_address_of_framed() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "inner_address_of_framed")
    }

    #[test]
    fn inline_fn() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "inline_fn")?;

        let asm = std::fs::read_to_string("src/tests/functional/inline_fn.asm").unwrap();
        assert!(
            asm.find("call fn_add").is_none(),
            "Found a call to `fn_add` in the generated assembly!"
        );
        assert!(
            asm.find("fn_add:").is_some(),
            "Didn't find any definition of `fn_add` in generated assembly"
        );
        Ok(())
    }

    #[test]
    fn pointer_offsets() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "pointer_offsets")
    }

    #[test]
    fn generic_fn_with_const_ptr_arg() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "generic_fn_with_const_ptr_arg")
    }

    #[test]
    fn valid_pointer_operations() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "valid_pointer_operations")
    }

    #[test]
    fn pointer_types() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "pointer_types")
    }

    #[test]
    fn mutable_fn_input() -> Result<(), std::io::Error> {
        super::test_tools::run_test("functional", "mutable_fn_input")
    }
}
