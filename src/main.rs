mod expr;
mod lex;
mod passes;
mod stmt;
mod types;
mod union_find;

use std::collections::HashMap;

use expr::{BinOp, Expr};
use stmt::{Function, Stmt};
use types::TypeVar;

use crate::{
    lex::LexExtension,
    passes::CType,
    types::{Type, TypeInference, Var},
};

fn example() -> Stmt<'static, (), LexExtension<'static>> {
    Stmt::function(Function::new(
        "main",
        [],
        [],
        Expr::block(
            [
                Expr::literal(0u32, ()),
                Expr::wile(
                    [
                        Expr::ext(LexExtension::Ident("dup"), ()),
                        Expr::literal(20u32, ()),
                        Expr::binop(BinOp::Lt, ()),
                    ],
                    Expr::block(
                        [
                            Expr::ext(LexExtension::As(vec!["i"]), ()),
                            Expr::ext(LexExtension::Ident("i"), ()),
                            Expr::print(()),
                            Expr::literal(": ", ()),
                            Expr::print_string(()),
                            Expr::ext(LexExtension::Ident("i"), ()),
                            Expr::ext(LexExtension::Ident("fib"), ()),
                            Expr::print(()),
                            Expr::literal("\\n", ()),
                            Expr::print_string(()),
                            Expr::ext(LexExtension::Ident("i"), ()),
                            Expr::literal(1u32, ()),
                            Expr::binop(BinOp::Add, ()),
                        ],
                        (),
                    ),
                    (),
                ),
                Expr::ext(LexExtension::Ident("drop"), ()),
            ],
            (),
        ),
    ))
}

/// fn fib(u64: n) -> [u64] {
///     n 2 < if {
///         1
///     } else {
///         n 1 - fib
///         n 2 - fib +
///     }
/// }
fn fib() -> Stmt<'static, (), LexExtension<'static>> {
    Stmt::function(Function::new(
        "fib",
        [Type::U32],
        [Type::U32],
        Expr::block(
            [
                Expr::ext(LexExtension::As(vec!["n"]), ()),
                Expr::ext(LexExtension::Ident("n"), ()),
                Expr::literal(2u32, ()),
                Expr::binop(BinOp::Lt, ()),
                Expr::iff(
                    Expr::ext(LexExtension::Ident("n"), ()),
                    Expr::block(
                        [
                            Expr::ext(LexExtension::Ident("n"), ()),
                            Expr::literal(1u32, ()),
                            Expr::binop(BinOp::Sub, ()),
                            Expr::ext(LexExtension::Ident("fib"), ()),
                            Expr::ext(LexExtension::Ident("n"), ()),
                            Expr::literal(2u32, ()),
                            Expr::binop(BinOp::Sub, ()),
                            Expr::ext(LexExtension::Ident("fib"), ()),
                            Expr::binop(BinOp::Add, ()),
                        ],
                        (),
                    ),
                    (),
                ),
            ],
            (),
        ),
    ))
}

fn dup(ctx: &mut TypeInference) -> Stmt<'static, (), LexExtension<'static>> {
    Stmt::function(Function::new_generic(
        "dup",
        [Type::Var(TypeVar(0))],
        [Type::Var(TypeVar(0)), Type::Var(TypeVar(0))],
        Expr::block(
            [
                Expr::ext(LexExtension::As(vec!["t"]), ()),
                Expr::ext(LexExtension::Ident("t"), ()),
                Expr::ext(LexExtension::Ident("t"), ()),
            ],
            (),
        ),
        ["T"],
        ctx,
    ))
}

fn haystack_drop(ctx: &mut TypeInference) -> Stmt<'static, (), LexExtension<'static>> {
    Stmt::function(Function::new_generic(
        "drop",
        [Type::Var(TypeVar(1))],
        [],
        Expr::ext(LexExtension::As(vec!["_"]), ()),
        ["T"],
        ctx,
    ))
}

fn string() -> Stmt<'static, (), LexExtension<'static>> {
    Stmt::strukt(
        "Str",
        vec![("size", Type::U32), ("string", Type::pointer(Type::U8))],
    )
}

fn main() {
    let mut inference = TypeInference::new();

    let stmts = vec![
        string(),
        example(),
        fib(),
        dup(&mut inference),
        haystack_drop(&mut inference),
    ];

    let (_types, fns) = stmts.into_iter().fold(
        (HashMap::new(), Vec::new()),
        |(mut types, mut fns), stmt| {
            match stmt {
                Stmt::Function(func) => fns.push(func),
                Stmt::Type(ty) => {
                    if types.insert(ty.name.to_owned(), ty).is_some() {
                        todo!("name conflict error")
                    }
                }
            }

            (types, fns)
        },
    );

    let mut env = HashMap::new();
    let mut fn_names = HashMap::new();
    let mut fn_idents = HashMap::new();
    fns.iter().enumerate().for_each(|(i, func)| {
        assert!(fn_names.insert(i, format!("{}", &func.name)).is_none());
        assert!(fn_idents.insert(format!("{}", &func.name), i).is_none());
        assert!(env.insert(Var::Func(i), Type::from(func)).is_none());
    });

    // Resolve identifiers into variables & function calls
    let fns = fns.into_iter().map(|f| f.resolve_names(&fn_idents));

    // Type check all functions.
    let mut fns = fns
        .map(|f| f.type_check(&mut inference, &env))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|(func, e)| format!("`{func}` failed to type check: {e:?}"))
        .unwrap();

    // Monomorphize generic function calls
    let fns = {
        let to_monomorphize =
            passes::find_monomorphizations(&mut fns, &env, &mut fn_names, &mut fn_idents);
        passes::resolve_monomorphizations(to_monomorphize, fns, &mut env, &fn_names, &mut inference)
    };

    // Transpile to c
    let fns = fns
        .into_iter()
        .map(|func| func.into_ssa_form(&env, &fn_names))
        .flatten()
        .collect::<Vec<_>>();

    generate!(0, "#include <stdio.h>");
    generate!(0, "#include <stdint.h>");
    generate!(0, "#include <stdbool.h>");
    generate!(0, "");

    let mut types = HashMap::from([("Str".to_owned(), CType::string())]);
    fns.iter().for_each(|f| {
        eprintln!("{}", f.name);
        types.extend(f.body.get_ctypes())
    });

    let types = CType::organize_dependencies(types.into_iter().map(|(_, v)| v)).unwrap();

    types.iter().for_each(|ty| ty.declare(0));
    generate!(0, "");
    types.iter().for_each(|ty| ty.transpile(0, 4));
    fns.iter().for_each(|f| f.declare(0, 4));
    fns.iter().for_each(|f| f.transpile(0, 4));
}
