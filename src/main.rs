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
    let mut parser = Parser::new()?;
    parser.parse(tokens)?;

    let statements = parser.finish();

    let (world, mut typestore, mut env, externs, entry) = TypeCheck::build_world(statements)?;
    let Some(entry) = entry else {
        eprintln!("No main function found. Exiting after type checking");
        return Ok(());
    };

    let (types, fns, entry) = world.monomorphize(&mut typestore, &mut env, entry);

    let mut s = String::new();
    CCodeGen::generate(&mut s, &mut typestore, &types, &fns, &env, &externs, entry).unwrap();

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
