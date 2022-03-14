mod compiler;
mod ir;
mod lex;
use clap::Parser;
use compiler::compile_haystack;

#[derive(Parser)]
struct Cli {
    file: String,
    #[clap(short, long)]
    run: bool,
    #[clap(long)]
    ir: bool,
    #[clap(long)]
    simple: bool,
}

fn main() {
    let cli = Cli::parse();
    compile_haystack(cli.file, cli.run, cli.ir, cli.simple);
}
