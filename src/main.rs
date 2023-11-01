use clap::Parser;

mod expression;
mod types;

use expression::{BlockExpr, Expr, LiteralExpr, VarExpr};

use crate::types::FnTy;

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

    let main_sig = FnTy::new([], []);

    dbg!(main_expr);
}
