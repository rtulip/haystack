use clap::Parser;

mod expression;
mod interpreter;
mod parser;
mod statement;
mod types;

use expression::{AddExpr, AsExpr, BlockExpr, IfExpr, LessThanExpr, LiteralExpr, SubExpr, VarExpr};
use interpreter::{Element, Interpreter};

use crate::{
    statement::FunctionStmt,
    types::{Context, FnTy, Scheme, Ty, TyGen},
};

#[derive(Parser)]
struct Cli {
    file: String,
    #[clap(short, long)]
    run: bool,
}

fn builtin_print_string() -> (&'static str, Element<'static>) {
    (
        "__builtin_print_string",
        Element::Extern(|interp: &mut Interpreter| {
            let s = interp.pop_str()?;
            print!("{s}");
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

fn puts() -> (&'static str, Element<'static>) {
    ("puts", VarExpr::from("__builtin_print_string").into())
}

fn putlns() -> (&'static str, Element<'static>) {
    (
        "putlns",
        BlockExpr::from([
            VarExpr::from("puts").into(),
            LiteralExpr::from("\n").into(),
            VarExpr::from("puts").into(),
        ])
        .into(),
    )
}

fn putb() -> (&'static str, Element<'static>) {
    ("putb", VarExpr::from("__builtin_print_bool").into())
}

fn putlnb() -> (&'static str, Element<'static>) {
    (
        "putlnb",
        BlockExpr::from([
            VarExpr::from("putb").into(),
            LiteralExpr::from("\n").into(),
            VarExpr::from("puts").into(),
        ])
        .into(),
    )
}

fn putu() -> (&'static str, Element<'static>) {
    ("putu", VarExpr::from("__builtin_print_u32").into())
}

fn putlnu() -> (&'static str, Element<'static>) {
    (
        "putlnu",
        BlockExpr::from([
            VarExpr::from("putu").into(),
            LiteralExpr::from("\n").into(),
            VarExpr::from("puts").into(),
        ])
        .into(),
    )
}

fn dup() -> (&'static str, Element<'static>) {
    (
        "dup",
        BlockExpr::from([
            AsExpr::from(["t"]).into(),
            VarExpr::from("t").into(),
            VarExpr::from("t").into(),
        ])
        .into(),
    )
}

fn fib() -> (&'static str, Element<'static>) {
    (
        "fib",
        BlockExpr::from([
            AsExpr::from(["n"]).into(),
            VarExpr::from("n").into(),
            LiteralExpr::from(2).into(),
            LessThanExpr.into(),
            IfExpr::new(
                VarExpr::from("n"),
                BlockExpr::from([
                    VarExpr::from("n").into(),
                    LiteralExpr::from(1).into(),
                    SubExpr.into(),
                    VarExpr::from("fib").into(),
                    VarExpr::from("n").into(),
                    LiteralExpr::from(2).into(),
                    SubExpr.into(),
                    VarExpr::from("fib").into(),
                    AddExpr.into(),
                ]),
            )
            .into(),
        ])
        .into(),
    )
}

fn main() {
    // let cli = Cli::parse();

    let main = FunctionStmt::new(
        BlockExpr::from([
            LiteralExpr::from("Hello World").into(),
            VarExpr::from("putlns").into(),
        ])
        .into(),
        Scheme::new([], FnTy::new([], [])),
    );

    let ctx = Context::from([("putlns", Scheme::new([], FnTy::new([Ty::Str], [])))]);
    let mut gen = TyGen::new();

    // FIXME:
    main.type_check(&ctx, &mut gen).unwrap();

    Interpreter::new([
        builtin_print_bool(),
        builtin_print_string(),
        builtin_print_u32(),
        putb(),
        puts(),
        putu(),
        putlnb(),
        putlns(),
        putlnu(),
        dup(),
        (
            "main",
            BlockExpr::from([
                LiteralExpr::from("Hello World").into(),
                VarExpr::from("dup").into(),
                VarExpr::from("putlns").into(),
                VarExpr::from("putlns").into(),
                LiteralExpr::from(5).into(),
                VarExpr::from("dup").into(),
                LiteralExpr::from(true).into(),
                IfExpr::new(
                    BlockExpr::from([
                        LiteralExpr::from("True Path").into(),
                        VarExpr::from("putlns").into(),
                    ]),
                    BlockExpr::from([
                        LiteralExpr::from(3).into(),
                        AddExpr.into(),
                        LiteralExpr::from("False Path").into(),
                        VarExpr::from("putlns").into(),
                    ]),
                )
                .into(),
                AddExpr.into(),
                VarExpr::from("putlnu").into(),
            ])
            .into(),
        ),
    ])
    .start("main")
    .unwrap();

    println!("=============================");

    Interpreter::new([
        builtin_print_bool(),
        builtin_print_string(),
        builtin_print_u32(),
        putb(),
        puts(),
        putu(),
        putlnb(),
        putlns(),
        putlnu(),
        dup(),
        fib(),
        (
            "main",
            BlockExpr::from([
                LiteralExpr::from(10).into(),
                VarExpr::from("fib").into(),
                VarExpr::from("putlnu").into(),
            ])
            .into(),
        ),
    ])
    .start("main")
    .unwrap();
}
