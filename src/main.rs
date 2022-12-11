#![feature(btree_drain_filter)]
#![feature(drain_filter)]
#![warn(missing_docs)]

//! Haystack documentation...

mod ast;
mod backend;
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
}

fn main() {
    let cli = Cli::parse();
    if let Err(e) = compile_haystack(cli.file, cli.run) {
        e.report();
        std::process::exit(1);
    }
}
