mod expr;
mod passes;
mod stmt;
mod types;
mod union_find;

use std::collections::HashMap;

use expr::Expr;
use stmt::Function;

use crate::types::{Stack, Type, TypeInference};

fn example() -> Expr<'static, (), ()> {
    Expr::block(
        [
            Expr::literal("Hello World!", ()),
            Expr::print_string(()),
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

    generate!(0, "typedef struct HaystackStr {{");
    generate!(4, "uint32_t size;");
    generate!(4, "uint8_t* string;");
    generate!(0, "}} HaystackStr;\n");
    generate!(0, "");


    generate!(0, "int main() {{");
    e.transpile(4, 4);
    generate!(0, "}}");
    
}
