#![allow(unused)]
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
    let file = "main.hay";

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

    let (types, fns) = world.monomorphize(&mut typestore, &mut env, entry);

    let mut s = String::new();
    CCodeGen::generate(&mut s, &mut typestore, &types, &fns, &externs).unwrap();

    println!("{s}");

    Ok(())
}
