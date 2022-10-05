use crate::ast::parser::Parser;
use crate::ast::stmt::Stmt;
use crate::error::HayError;
use crate::lex::scanner::Scanner;
use crate::lex::token::Loc;
use std::io::{self, Write};
use std::process::{Command, Output};

pub fn compile_haystack(
    input_path: String,
    _run: bool,
    _ir: bool,
    _simple: bool,
) -> Result<Vec<Stmt>, HayError> {
    if let Ok(source) = std::fs::read_to_string(&input_path) {
        let scanner = Scanner::new(&input_path, &source);
        let tokens = scanner.scan_tokens()?;
        let parser = Parser::new(tokens);
        let stmts = parser.parse()?;

        Ok(stmts)
    } else {
        HayError::new(
            format!("Failed to read from file: {input_path}"),
            Loc::new(input_path, 0, 0, 0),
        )
    }
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
}
