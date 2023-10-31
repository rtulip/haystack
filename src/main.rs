use clap::Parser;

#[derive(Parser)]
struct Cli {
    file: String,
    #[clap(short, long)]
    run: bool,
}

fn main() {
    let cli = Cli::parse();
}
