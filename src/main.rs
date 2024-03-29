mod expr;
mod lex;
mod passes;
mod stmt;
mod types;
mod union_find;

use std::collections::HashMap;

use expr::{BinOp, Expr};
use stmt::Function;
use types::TypeVar;

use crate::{
    lex::LexExtension,
    passes::CType,
    types::{Type, TypeInference, Var},
};

fn example() -> Function<'static, (), LexExtension<'static>> {
    Function::new(
        "main",
        [],
        [],
        Expr::block(
            [
                Expr::ext(LexExtension::Ident("two_ints"), ()),
                Expr::ext(LexExtension::Ident("my_add"), ()),
                Expr::print(()),
                Expr::ext(LexExtension::Ident("hello"), ()),
                Expr::literal(2u32, ()),
                Expr::ext(LexExtension::Ident("even_odd"), ()),
                Expr::print(()),
                Expr::literal(0u32, ()),
                Expr::ext(LexExtension::Ident("fib"), ()),
                Expr::print(()),
                Expr::literal(1u32, ()),
                Expr::ext(LexExtension::Ident("fib"), ()),
                Expr::print(()),
                Expr::literal(2u32, ()),
                Expr::ext(LexExtension::Ident("fib"), ()),
                Expr::print(()),
                Expr::literal(3u32, ()),
                Expr::ext(LexExtension::Ident("fib"), ()),
                Expr::print(()),
                Expr::literal(4u32, ()),
                Expr::ext(LexExtension::Ident("fib"), ()),
                Expr::print(()),
                Expr::literal(5u32, ()),
                Expr::ext(LexExtension::Ident("fib"), ()),
                Expr::print(()),
                Expr::literal(6u32, ()),
                Expr::ext(LexExtension::Ident("fib"), ()),
                Expr::print(()),
                Expr::literal(7u32, ()),
                Expr::ext(LexExtension::Ident("fib"), ()),
                Expr::print(()),
                Expr::ext(LexExtension::Ident("bind_test"), ()),
                Expr::ext(LexExtension::Ident("gen_call"), ()),
            ],
            (),
        ),
    )
}

fn my_add() -> Function<'static, (), LexExtension<'static>> {
    Function::new(
        "my_add",
        [Type::U32, Type::U32],
        [Type::U32],
        Expr::binop(BinOp::Add, ()),
    )
}

fn two_ints() -> Function<'static, (), LexExtension<'static>> {
    Function::new(
        "two_ints",
        [],
        [Type::U32, Type::U32],
        Expr::block([Expr::literal(1u32, ()), Expr::literal(2u32, ())], ()),
    )
}

fn hello_world() -> Function<'static, (), LexExtension<'static>> {
    Function::new(
        "hello",
        [],
        [],
        Expr::block(
            [Expr::literal("Hello World", ()), Expr::print_string(())],
            (),
        ),
    )
}

fn even_odd() -> Function<'static, (), LexExtension<'static>> {
    Function::new(
        "even_odd",
        [Type::U32],
        [Type::U32],
        Expr::block(
            [
                Expr::literal(2u32, ()),
                Expr::binop(BinOp::Mod, ()),
                Expr::literal(0u32, ()),
                Expr::binop(BinOp::Eq, ()),
                Expr::iff(Expr::literal(123u32, ()), Expr::literal(321u32, ()), ()),
            ],
            (),
        ),
    )
}

/// fn fib(u64: n) -> [u64] {
///     n 2 < if {
///         1
///     } else {
///         n 1 - fib
///         n 2 - fib +
///     }
/// }
fn fib() -> Function<'static, (), LexExtension<'static>> {
    Function::new(
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
                    Expr::literal(1u32, ()),
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
    )
}

fn bind_test() -> Function<'static, (), LexExtension<'static>> {
    // {
    //     "outer" as [x]
    //     x print
    //     {
    //          12345 as [x]
    //          x print
    //     }
    //     x print
    // }
    Function::new(
        "bind_test",
        [],
        [],
        Expr::block(
            [
                Expr::literal("outer", ()),
                Expr::ext(LexExtension::As(vec!["x"]), ()),
                Expr::ext(LexExtension::Ident("x"), ()),
                Expr::print_string(()),
                Expr::block(
                    [
                        Expr::literal(12345u32, ()),
                        Expr::ext(LexExtension::As(vec!["x"]), ()),
                        Expr::ext(LexExtension::Ident("x"), ()),
                        Expr::print(()),
                    ],
                    (),
                ),
                Expr::ext(LexExtension::Ident("x"), ()),
                Expr::print_string(()),
            ],
            (),
        ),
    )
}

fn dup(ctx: &mut TypeInference) -> Function<'static, (), LexExtension<'static>> {
    Function::new_generic(
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
    )
}

fn gen_call() -> Function<'static, (), LexExtension<'static>> {
    Function::new(
        "gen_call",
        [],
        [],
        Expr::block(
            [
                Expr::literal("dupped", ()),
                Expr::ext(LexExtension::Ident("dup"), ()),
                Expr::print_string(()),
                Expr::print_string(()),
                Expr::literal(12345u32, ()),
                Expr::ext(LexExtension::Ident("dup"), ()),
                Expr::print(()),
                Expr::print(()),
            ],
            (),
        ),
    )
}

fn main() {
    let mut inference = TypeInference::new();
    let fns = vec![
        example(),
        my_add(),
        two_ints(),
        hello_world(),
        even_odd(),
        fib(),
        bind_test(),
        dup(&mut inference),
        gen_call(),
    ];

    let mut env = HashMap::new();
    let mut fn_names = HashMap::new();
    let mut fn_idents = HashMap::new();
    fns.iter().enumerate().for_each(|(i, func)| {
        assert!(fn_names.insert(i, format!("{}", &func.name)).is_none());
        assert!(fn_idents.insert(format!("{}", &func.name), i).is_none());
        assert!(env.insert(Var::Func(i), Type::from(func)).is_none());
    });

    let fns = fns.into_iter().map(|f| f.resolve_names(&fn_idents));

    let mut fns = fns
        .map(|f| f.type_check(&mut inference, &env))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|(func, e)| format!("`{func}` failed to type check: {e:?}"))
        .unwrap();

    let to_monomorphise = fns
        .iter_mut()
        .map(|f| f.find_monomorphizations(&env, &mut fn_idents, &mut fn_names))
        .flatten()
        .collect::<Vec<_>>();

    let monomorphized = to_monomorphise
        .into_iter()
        .map(|(var, new_idx, ty)| {
            let idx = match var {
                Var::Func(idx) => idx,
                Var::Ident(_) => unreachable!(),
            };

            let fn_ty = ty.var_ty.unwrap().expect_function();

            let body = fns.get(idx).unwrap().body.clone().map_meta(|(m, _)| m);

            let new_f = Function::new(
                fn_names.get(&new_idx).unwrap(),
                fn_ty.input,
                fn_ty.output,
                body,
            );

            assert!(env.insert(Var::Func(new_idx), Type::from(&new_f)).is_none());
            new_f
        })
        .collect::<Vec<_>>();

    let monomorphized = monomorphized
        .into_iter()
        .map(|func| {
            func.type_check(&mut inference, &env)
                .expect("Monomorphized functions should type check")
        })
        .collect::<Vec<_>>();

    fns.extend(monomorphized);

    let fns = fns
        .into_iter()
        .map(|func| func.into_ssa_form(&env, &fn_names))
        .flatten()
        .collect::<Vec<_>>();

    generate!(0, "#include <stdio.h>");
    generate!(0, "#include <stdint.h>");
    generate!(0, "#include <stdbool.h>");
    generate!(0, "");

    let mut types = HashMap::new();
    fns.iter().for_each(|f| types.extend(f.body.get_ctypes()));

    let types = CType::organize_dependencies(types.into_iter().map(|(_, v)| v)).unwrap();

    types.iter().for_each(|ty| ty.declare(0));
    generate!(0, "");
    types.iter().for_each(|ty| ty.transpile(0, 4));
    fns.iter().for_each(|f| f.declare(0, 4));
    fns.iter().for_each(|f| f.transpile(0, 4));
}
