#![allow(unused)]
use crate::{
    error::HayError,
    lex::{scanner::Scanner, token::Loc},
    parser::parser::Parser,
    typecheck::TypeCheck,
};

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

    let (world, mut typestore, mut env, entry) = TypeCheck::build_world(statements)?;
    let Some(entry) = entry else {
        eprintln!("No main function found. Exiting after type checking");
        return Ok(());
    };

    let (types, fns) = world.monomorphize(&mut typestore, &mut env, entry);

    for tid in types {
        eprintln!("{tid:?}: {}", typestore.fmt(tid))
    }

    for (var, f) in fns {
        eprintln!(
            "{}: {var:?} {}",
            f.expr.meta.0.loc,
            typestore.fmt(f.expr.meta.1)
        );
    }

    Ok(())
}
