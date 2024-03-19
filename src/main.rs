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
    types::{Stack, Type, TypeInference},
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

fn main() {
    let mut inference = TypeInference::new();
    let main_fn = example()
        .type_check(&mut inference, &HashMap::new())
        .unwrap()
        .into_ssa_form();

    generate!(0, "#include <stdio.h>");
    generate!(0, "#include <stdint.h>");
    generate!(0, "#include <stdbool.h>");
    generate!(0, "");

    CType::string().transpile(0, 4);

    main_fn.transpile(0, 4);
}
