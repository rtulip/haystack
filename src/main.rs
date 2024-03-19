mod expr;
mod passes;
mod stmt;
mod types;
mod union_find;

use std::collections::HashMap;

use expr::Expr;
use stmt::Function;

use crate::{
    passes::CType,
    types::{Stack, TypeInference},
};

fn example() -> Expr<'static, (), ()> {
    Expr::block(
        [
            Expr::literal(12345u32, ()),
            Expr::block(
                [
                    Expr::literal("Hello World!", ()),
                    Expr::literal(54321u32, ()),
                    Expr::print(()),
                ],
                (),
            ),
            Expr::print_string(()),
            Expr::print(()),
        ],
        (),
    )
}

fn main() {
    let mut inference = TypeInference::new();
    let e = example();

    let e = inference
        .type_check(e, &HashMap::new(), &mut Stack::new(), vec![])
        .unwrap()
        .0
        .into_ssa_form(Stack::new());

    generate!(0, "#include <stdio.h>");
    generate!(0, "#include <stdint.h>");
    generate!(0, "#include <stdbool.h>");
    generate!(0, "");

    CType::string().transpile(0, 4);

    generate!(0, "int main() {{");
    e.transpile(4, 4);
    generate!(0, "}}");
}
