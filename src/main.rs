use clap::Parser;

mod expression;
mod statement;
mod types;

use expression::{ApplicationError, BlockExpr, Expr, LiteralExpr, VarExpr};

use crate::{
    statement::FunctionStmt,
    types::{Context, FnTy, Scheme, Stack, Ty, TyGen},
};

#[derive(Parser)]
struct Cli {
    file: String,
    #[clap(short, long)]
    run: bool,
}

fn main() {
    // let cli = Cli::parse();

    let main = FunctionStmt::new(
        BlockExpr::from([
            LiteralExpr::from("Hello World").into(),
            VarExpr::from("println").into(),
        ])
        .into(),
        Scheme::new([], FnTy::new([], [])),
    );

    let mut ctx = Context::from([("println", Scheme::new([], FnTy::new([Ty::Str], [])))]);
    let mut gen = TyGen::new();

    let out = main.type_check(&ctx, &mut gen);

    dbg!(out);
}
