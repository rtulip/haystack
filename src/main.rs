#![feature(btree_drain_filter)]

mod ast;
mod compiler;
mod error;
mod lex;
mod types;
use clap::Parser;
use compiler::compile_haystack;

#[derive(Parser)]
struct Cli {
    file: String,
    #[clap(short, long)]
    run: bool,
    #[clap(long)]
    json: bool,
    #[clap(long)]
    simple: bool,
}

fn main() {
    let cli = Cli::parse();
    if let Err(e) = compile_haystack(cli.file, cli.run, cli.json, cli.simple) {
        e.report();
        std::process::exit(1);
    }
}
