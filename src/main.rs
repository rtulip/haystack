use clap::Parser;

mod expression;
mod interpreter;
mod parser;
mod statement;
mod types;

use interpreter::{Element, Interpreter};

use crate::{
    parser::scanner::Scanner,
    types::{Context, TyGen},
};

#[derive(Parser)]
struct Cli {
    file: String,
    #[clap(short, long)]
    run: bool,
}

fn unescape_string(s: &str) -> String {
    let mut chars = s.chars();
    let mut out = String::new();

    while let Some(c) = chars.next() {
        match c {
            '\\' => match chars.next() {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('r') => out.push('\r'),
                Some('0') => out.push('\0'),
                Some(c) => panic!("Unknown excape character: `\\{c}`"),
                None => panic!("Expected a chacter to escape, but found none"),
            },
            c => out.push(c),
        }
    }

    out
}

fn builtin_print_string() -> (&'static str, Element<'static>) {
    (
        "__builtin_print_string",
        Element::Extern(|interp: &mut Interpreter| {
            let s = interp.pop_str()?;
            print!("{}", unescape_string(s));
            Ok(())
        }),
    )
}

fn builtin_print_bool() -> (&'static str, Element<'static>) {
    (
        "__builtin_print_bool",
        Element::Extern(|interp: &mut Interpreter| {
            let b = interp.pop_bool()?;
            print!("{b}");
            Ok(())
        }),
    )
}

fn builtin_print_u32() -> (&'static str, Element<'static>) {
    (
        "__builtin_print_u32",
        Element::Extern(|interp: &mut Interpreter| {
            let n = interp.pop_u32()?;
            print!("{n}");
            Ok(())
        }),
    )
}

fn main() {
    // let cli = Cli::parse();

    let file = "test.hay";
    let source = std::fs::read_to_string(file).unwrap();

    let tokens = Scanner::scan_tokens(file, &source).unwrap();
    let functions = crate::parser::Parser::parse(tokens).unwrap();
    let context = Context::from_functions(&functions);
    let mut gen = TyGen::new();

    for func in &functions {
        func.type_check(&context, &mut gen).unwrap();
    }

    let mut elements = vec![
        builtin_print_bool(),
        builtin_print_string(),
        builtin_print_u32(),
    ];
    elements.extend(functions.iter().map(|func| {
        (
            func.token.quote().as_str(),
            Element::Expr(func.expr.clone()),
        )
    }));

    let interpreter = Interpreter::new(elements);

    interpreter.start("main").unwrap();
}
