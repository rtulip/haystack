mod expr;
mod stmt;

use expr::Expr;
use stmt::{Function, Stmt};

fn example() -> Stmt<'static, (), (), ()> {
    Stmt::function(
        Function::new(
            "main",
            Expr::block([Expr::literal(12345, ()), Expr::print(())], ()),
        ),
        (),
    )
}

fn main() {
    let e = example();

    println!("Hello World!");
}
