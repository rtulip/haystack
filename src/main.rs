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
            Expr::literal(3u8, ()),
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

    println!("#include <stdio.h>");
    println!("#include <stdint.h>");
    println!("#include <stdbool.h>");
    
    println!("int main() {{");
    e.transpile(4, 4);
    println!("}}");
    
}
