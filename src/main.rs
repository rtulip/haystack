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
        [Type::U32],
        Expr::block(
            [
                Expr::literal(420u32, ()),
                Expr::literal(69u32, ()),
                Expr::binop(BinOp::Mul, ()),
                Expr::print(()),
                Expr::literal(223u32, ()),
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

fn main() {
    let mut inference = TypeInference::new();
    let fns = vec![example(), my_add()];

    let mut env = HashMap::new();
    fns.iter().enumerate().for_each(|(i, func)| {
        assert!(env.insert(Var::Func(i), Type::from(func)).is_none());
    });

    let fns = fns
        .into_iter()
        .map(|f| f.type_check(&mut inference, &env).unwrap().into_ssa_form())
        .collect::<Vec<_>>();

    generate!(0, "#include <stdio.h>");
    generate!(0, "#include <stdint.h>");
    generate!(0, "#include <stdbool.h>");
    generate!(0, "");

    CType::string().transpile(0, 4);

    fns.iter().for_each(|f| f.transpile(0, 4));
}
