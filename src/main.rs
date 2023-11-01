use clap::Parser;

mod expression;

use expression::{BlockExpr, Expr, LiteralExpr, VarExpr};

#[derive(Parser)]
struct Cli {
    file: String,
    #[clap(short, long)]
    run: bool,
}

fn main() {
    // let cli = Cli::parse();

    let main_expr: Expr<'_> = BlockExpr::from([
        LiteralExpr::from("Hello World").into(),
        VarExpr::from("println").into(),
    ])
    .into();

    dbg!(main_expr);
}
