#![allow(unused)]
use std::{format, io::Write, path::Path};

use crate::{
    codegen::CCodeGen,
    error::HayError,
    lex::{scanner::Scanner, token::Loc},
    parser::parser::Parser,
    typecheck::TypeCheck,
};

mod codegen;
mod error;
mod lex;
mod parser;
mod typecheck;

fn main() -> Result<(), HayError> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <File> [-r/--run]", args[0]);
        std::process::exit(1);
    }

    let file = Path::new(&args[1]);
    let filename = file.file_stem().unwrap();

    let source = std::fs::read_to_string(file).map_err(|e| {
        HayError::new(
            "Failed to open file",
            Loc::new(file.to_str().unwrap(), 1, 0, 0),
        )
        .with_hint(format!("{e}"))
    })?;

    let tokens = Scanner::new(file.to_str().unwrap(), &source).scan_tokens()?;
    let statements = Parser::parse(tokens)?;

    let (world, mut typestore, mut env, externs, entry) = TypeCheck::build_world(statements)?;
    let Some(entry) = entry else {
        eprintln!("No main function found. Exiting after type checking");
        return Ok(());
    };

    let (types, fns, entry) = world.monomorphize(&mut typestore, &mut env, entry);

    let mut s = String::new();
    CCodeGen::generate(&mut s, &mut typestore, &types, &fns, &externs, entry).unwrap();

    let c_exe_path = Path::new("target/").join(filename);
    let c_source_path = c_exe_path.with_extension("c");

    std::fs::write(&c_source_path, &s).unwrap();
    let output = std::process::Command::new("gcc")
        .args([
            c_source_path.to_str().unwrap(),
            "-g",
            "-o",
            c_exe_path.to_str().unwrap(),
        ])
        .output()
        .unwrap();

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        eprintln!("GCC Error:\n{}", stderr);
        assert!(false);
    }

    if (args.len() >= 3 && (args[2] == "-r" || args[2] == "--run")) {
        let output = std::process::Command::new(&format!("./{}", c_exe_path.display()))
            .output()
            .unwrap();
        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            print!("{}", stdout);
        }
    } else {
        eprintln!("All done");
    }

    Ok(())
}

#[cfg(test)]
mod compilation_test {
    use core::panic;
    use std::{
        fmt::{Debug, Display},
        format,
        path::Path,
        process::{Command, Output},
        str::FromStr,
    };

    struct Cleanup(std::path::PathBuf);

    impl Drop for Cleanup {
        fn drop(&mut self) {
            if let Err(err) = std::fs::remove_file(&self.0) {
                if err.kind() != std::io::ErrorKind::NotFound {
                    eprintln!("Failed to clean up {}: {err}", self.0.display());
                }
            }
        }
    }

    fn assert_snapshot(path: &Path, actual: &str) {
        let normalized = actual.replace("\r\n", "\n");

        if !path.exists() {
            std::fs::write(path, actual).unwrap();
            println!("Created snapshot: {}", path.display());
            return;
        }

        let expected = std::fs::read_to_string(path).unwrap();
        let expected = expected.replace("\r\n", "\n");

        assert_eq!(
            normalized,
            expected,
            "Snapshot mismatch: {}",
            path.display()
        );
    }

    fn run_test(path: impl AsRef<Path>) {
        let path = path.as_ref();

        let executable = path.with_extension("");
        let _cleanup = Cleanup(executable);

        let output = Command::new("target/debug/haystack")
            .arg(path)
            .arg("--run")
            .output()
            .expect("Failed to execute compiler");

        let stdout = String::from_utf8(output.stdout).unwrap();
        let stderr = String::from_utf8(output.stderr).unwrap();
        let exitcode = output.status.code().unwrap();

        assert_snapshot(&path.with_extension("stdout"), &stdout);
        assert_snapshot(&path.with_extension("stderr"), &stderr);
        assert_snapshot(&path.with_extension("exitcode"), &exitcode.to_string());
    }

    #[test]
    fn add() {
        run_test("src/tests/functional/add.hay");
    }

    #[test]
    fn bad_return() {
        run_test("src/tests/typecheck/bad_return.hay");
    }

    #[test]
    fn conflicting_impl() {
        run_test("src/tests/typecheck/conflicting_impl.hay");
    }

    #[test]
    fn bad_number() {
        run_test("src/tests/scanning/bad_number.hay");
    }

    #[test]
    fn bad_u8() {
        run_test("src/tests/scanning/bad_u8.hay");
    }

    #[test]
    fn u8() {
        run_test("src/tests/scanning/u8.hay");
    }

    #[test]
    fn hex() {
        run_test("src/tests/scanning/hex.hay");
    }

    #[test]
    fn octal() {
        run_test("src/tests/scanning/octal.hay");
    }

    #[test]
    fn binary() {
        run_test("src/tests/scanning/binary.hay");
    }

    #[test]
    fn char() {
        run_test("src/tests/scanning/char.hay");
    }

    #[test]
    fn unknown_escaped_char() {
        run_test("src/tests/scanning/unknown_escaped_char.hay");
    }

    #[test]
    fn unterminated_char() {
        run_test("src/tests/scanning/unterminated_char.hay");
    }

    #[test]
    fn unterminated_string() {
        run_test("src/tests/scanning/unterminated_string.hay");
    }

    #[test]
    fn unexpected_character() {
        run_test("src/tests/scanning/unexpected_character.hay");
    }

    #[test]
    fn unknown_equals() {
        run_test("src/tests/scanning/unknown_equals.hay");
    }

    #[test]
    fn bad_const_pointer() {
        run_test("src/tests/parsing/bad_const_pointer.hay");
    }

    #[test]
    fn pointer_missing_type() {
        run_test("src/tests/parsing/pointer_missing_type.hay");
    }

    #[test]
    fn tuple_type() {
        run_test("src/tests/parsing/tuple_type.hay");
    }

    #[test]
    fn empty_tuple() {
        run_test("src/tests/parsing/empty_tuple.hay");
    }
}
