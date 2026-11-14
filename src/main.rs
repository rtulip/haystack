#![allow(unused)]
use std::{format, io::Write};

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

    let file = &args[1];
    let file_stripped = file.strip_suffix(".hay").unwrap();

    let source = std::fs::read_to_string(file).map_err(|e| {
        HayError::new("Failed to open file", Loc::new(file, 1, 0, 0)).with_hint(format!("{e}"))
    })?;

    let tokens = Scanner::new(file, &source).scan_tokens()?;
    let statements = Parser::parse(tokens)?;

    let (world, mut typestore, mut env, externs, entry) = TypeCheck::build_world(statements)?;
    let Some(entry) = entry else {
        eprintln!("No main function found. Exiting after type checking");
        return Ok(());
    };

    let (types, fns, entry) = world.monomorphize(&mut typestore, &mut env, entry);

    let mut s = String::new();
    CCodeGen::generate(&mut s, &mut typestore, &types, &fns, &externs, entry).unwrap();

    std::fs::write("target/out.c", &s).unwrap();
    let output = std::process::Command::new("gcc")
        .args(["target/out.c", "-g", "-o", file_stripped])
        .output()
        .unwrap();

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        eprintln!("GCC Error:\n{}", stderr);
        assert!(false);
    }

    if (args.len() >= 3 && (args[2] == "-r" || args[2] == "--run")) {
        let output = std::process::Command::new(&format!("./{file_stripped}"))
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
    use std::{fmt::{Debug, Display}, format, path::Path, process::{Command, Output}, str::FromStr};

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

    fn assert_snapshot(path: &Path, actual: &str)
    {
        if !path.exists() {
            std::fs::write(path, actual).unwrap();
            println!("Created snapshot: {}", path.display());
            return;
        }

        let expected = std::fs::read_to_string(path).unwrap();

        assert_eq!(
            actual,
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
        assert_snapshot(&path.with_extension("exitcode"),&exitcode.to_string());
    }

    #[test]
    fn add() {    
        run_test("src/tests/functional/add.hay");
    }

    #[test]
    fn bad_return() {    
        run_test("src/tests/typecheck/bad_return.hay");
    }
}
