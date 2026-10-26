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

    TypeCheck::build_world(statements)?;

    Ok(())
}
