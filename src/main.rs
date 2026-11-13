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
    let output = std::process::Command::new("gcc").args([
        "target/out.c", "-g", "-o", file_stripped
    ]).output().unwrap();

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        eprintln!("GCC Error:\n{}", stderr);
        assert!(false);
    }

    println!("-------------------------");

    if (args.len() >= 3 && (args[2] == "-r" || args[2] == "--run"))
    {
        let output = std::process::Command::new(&format!("./{file_stripped}")).output().unwrap();
        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            print!("{}", stdout);
        }
    }
    else {
        eprintln!("All done");
    }
    
    Ok(())
}

#[cfg(test)]
mod compilation_test {

    #[test]
    fn example() {
        assert!(std::process::Command::new("./target/debug/haystack").args(["main.hay", "--run"]).output().unwrap().status.success())
    }

}