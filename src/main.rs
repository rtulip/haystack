use std::{collections::HashMap, fmt::Debug, process::exit, rc::Rc};

use clap::Parser;

mod expression;
mod interpreter;
mod parser;
mod statement;
mod types;

use interpreter::{Element, Interpreter};
use types::Ty;

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

fn builtin_print_string() -> (String, Element<'static>) {
    (
        "__builtin_print_string".to_owned(),
        Element::Extern(|interp: &mut Interpreter| {
            let s = interp.pop_str()?;
            print!("{}", unescape_string(s));
            Ok(())
        }),
    )
}

fn builtin_print_bool() -> (String, Element<'static>) {
    (
        "__builtin_print_bool".to_owned(),
        Element::Extern(|interp: &mut Interpreter| {
            let b = interp.pop_bool()?;
            print!("{b}");
            Ok(())
        }),
    )
}

fn builtin_print_u32() -> (String, Element<'static>) {
    (
        "__builtin_print_u32".to_owned(),
        Element::Extern(|interp: &mut Interpreter| {
            let n = interp.pop_u32()?;
            print!("{n}");
            Ok(())
        }),
    )
}

trait Report<T> {
    fn report(self) -> T;
}
impl<T, E> Report<T> for Result<T, E>
where
    E: Debug,
{
    fn report(self) -> T {
        match self {
            Ok(ok) => ok,
            Err(err) => {
                eprintln!("{err:?}");
                exit(1)
            }
        }
    }
}

fn main() {
    // let cli = Cli::parse();

    let file = "test.hay";
    let source = std::fs::read_to_string(file).report();

    let mut gen = TyGen::new();
    let mut types = HashMap::from([("u32", Ty::U32), ("bool", Ty::Bool), ("Str", Ty::Str)]);

    let tokens = Scanner::scan_tokens(file, &source).report();
    let stmts = crate::parser::Parser::parse(tokens, &mut types, &mut gen).report();

    let mut functions = vec![];
    let mut context = Context::new();

    let mut elements = vec![
        builtin_print_bool(),
        builtin_print_string(),
        builtin_print_u32(),
    ];

    for stmt in stmts {
        match stmt {
            statement::Stmt::Function(f) => {
                let ident = f.token.quote().as_str();
                context.push(ident, f.scheme().clone());
                functions.push((ident.to_owned(), f));
            }
            statement::Stmt::TypeDef(_) => (),
            statement::Stmt::Impl(impl_) => {
                let prefix = format!("{}", impl_.token.quote().as_str());
                for f in impl_.fns {
                    let ident = format!("{prefix}.{}", f.token.quote().as_str());
                    context.push(ident.clone(), f.scheme().clone());
                    functions.push((ident, f));
                }
            }
        }
    }

    for (_, function) in &mut functions {
        function.resolve_names(&types, &context).unwrap();
    }

    for (_, function) in &functions {
        function.type_check(&types, &context, &mut gen).report();
    }

    elements.extend(
        functions
            .into_iter()
            .map(|(ident, func)| (ident, Element::Expr(func.expr.clone()))),
    );

    let interpreter = Interpreter::new(elements, &types);

    interpreter.start("main").report();
}
