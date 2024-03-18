mod expr;
mod stmt;
mod types;
mod union_find;

use std::collections::HashMap;

use expr::Expr;

use crate::types::{Stack, TypeInference};

fn example() -> Expr<'static, (),()> {
    Expr::block([Expr::literal(12345, ()), Expr::print(())], ())
}

fn main() {
    let mut inference = TypeInference::new();
    let e = example();

    let (e, scheme) = inference.type_check(e, &HashMap::new(), &mut Stack::new(), Stack::new()).unwrap();
    
}
