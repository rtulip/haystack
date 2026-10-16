use crate::{error::HayError, lex::scanner::Scanner, parser::parser::Parser};

mod error;
mod lex;
mod parser;

fn main() -> Result<(), HayError> {
    let source = "     fn main() { 12345 println }";

    let mut scanner = Scanner::new("example.hay", source);
    let tokens = scanner.scan_tokens()?;
    let statements = Parser::parse(tokens)?;

    Ok(())
}
