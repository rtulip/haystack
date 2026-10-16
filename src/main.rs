use crate::{error::HayError, lex::scanner::Scanner};

mod lex;
mod error;

fn main() -> Result<(), HayError>{

    let source =
"
fn main() { 12345 println }
";

    let mut scanner = Scanner::new("example.hay", source);
    let tokens = scanner.scan_tokens()?;

    for tok in tokens {
        println!("{tok}");
    }


    Ok(())
}