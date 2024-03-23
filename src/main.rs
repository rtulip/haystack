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

fn main() {
    let mut inference = TypeInference::new();
    let fns = vec![example(), my_add(), two_ints()];

    let mut env = HashMap::new();
    let mut fn_names = HashMap::new();
    fns.iter().enumerate().for_each(|(i, func)| {
        assert!(fn_names.insert(i, format!("{}", &func.name)).is_none());
        assert!(env.insert(Var::Func(i), Type::from(func)).is_none());
    });

    let fns = fns
        .into_iter()
        .map(|f| {
            f.type_check(&mut inference, &env)
                .unwrap()
                .into_ssa_form(&env, &fn_names)
        })
        .collect::<Vec<_>>();

    generate!(0, "#include <stdio.h>");
    generate!(0, "#include <stdint.h>");
    generate!(0, "#include <stdbool.h>");
    generate!(0, "");

    CType::string().transpile(0, 4);
    CType::Tuple(vec![CType::U32, CType::U32]).transpile(0, 4);

    fns.iter().for_each(|f| f.declare(0, 4));
    fns.iter().for_each(|f| f.transpile(0, 4));
}
