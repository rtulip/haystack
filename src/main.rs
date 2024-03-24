mod expr;
mod passes;
mod stmt;
mod types;
mod union_find;

use std::collections::HashMap;

use expr::{BinOp, Expr};
use stmt::Function;

use crate::{
    passes::CType,
    types::{Stack, Type, TypeInference, Var},
};

fn example() -> Function<'static, (), ()> {
    Function::new(
        "main",
        [],
        [],
        Expr::block(
            [
                Expr::call(Var::Func(2), ()),
                Expr::call(Var::Func(1), ()),
                Expr::print(()),
                Expr::call(Var::Func(3), ()),
                Expr::literal(2u32, ()),
                Expr::call(Var::Func(4), ()),
                Expr::print(()),
            ],
            (),
        ),
    )
}

fn my_add() -> Function<'static, (), ()> {
    Function::new(
        "my_add",
        [Type::U32, Type::U32],
        [Type::U32],
        Expr::binop(BinOp::Add, ()),
    )
}

fn two_ints() -> Function<'static, (), ()> {
    Function::new(
        "two_ints",
        [],
        [Type::U32, Type::U32],
        Expr::block([Expr::literal(1u32, ()), Expr::literal(2u32, ())], ()),
    )
}

fn hello_world() -> Function<'static, (), ()> {
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

fn even_odd() -> Function<'static, (), ()> {
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

fn main() {
    let mut inference = TypeInference::new();
    let fns = vec![example(), my_add(), two_ints(), hello_world(), even_odd()];

    let mut env = HashMap::new();
    let mut fn_names = HashMap::new();
    fns.iter().enumerate().for_each(|(i, func)| {
        assert!(fn_names.insert(i, format!("{}", &func.name)).is_none());
        assert!(env.insert(Var::Func(i), Type::from(func)).is_none());
    });

    let fns = fns
        .into_iter()
        .map(|f| {
            let name = f.name;
            f.type_check(&mut inference, &env)
                .expect(&format!("function `{name}` didn't type check"))
                .into_ssa_form(&env, &fn_names)
        })
        .collect::<Vec<_>>();

    generate!(0, "#include <stdio.h>");
    generate!(0, "#include <stdint.h>");
    generate!(0, "#include <stdbool.h>");
    generate!(0, "");

    let mut types = HashMap::new();
    fns.iter().for_each(|f| types.extend(f.body.get_ctypes()));

    types.values().for_each(|ty| ty.declare(0));
    generate!(0, "");
    types.values().for_each(|ty| ty.transpile(0, 4));
    fns.iter().for_each(|f| f.declare(0, 4));
    fns.iter().for_each(|f| f.transpile(0, 4));
}
