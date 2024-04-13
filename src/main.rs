mod c;
mod expr;
mod lex;
mod passes;
mod stmt;
mod types;
mod union_find;

use std::collections::HashMap;

use c::CType;
use expr::{BinOp, Expr};
use stmt::{Function, Stmt};
use types::TypeVar;

use crate::{
    lex::LexExtension,
    types::{Type, TypeInference, Var},
};

fn fib() -> Stmt<'static, Type, (), LexExtension<'static>> {
    Stmt::function(Function::new(
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
                    Expr::ext(LexExtension::Ident("n"), ()),
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
    ))
}

fn dup(ctx: &mut TypeInference) -> Stmt<'static, Type, (), LexExtension<'static>> {
    Stmt::function(Function::new_generic(
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
    ))
}

fn haystack_drop(ctx: &mut TypeInference) -> Stmt<'static, Type, (), LexExtension<'static>> {
    Stmt::function(Function::new_generic(
        "drop",
        [Type::Var(TypeVar(1))],
        [],
        Expr::ext(LexExtension::As(vec!["_"]), ()),
        ["T"],
        ctx,
    ))
}

fn printf() -> Stmt<'static, Type, (), LexExtension<'static>> {
    Stmt::xtern(
        "printf",
        [CType::pointer(CType::Char), CType::Variadic],
        CType::Int,
    )
}

fn foo() -> Stmt<'static, Type, (), LexExtension<'static>> {
    Stmt::xtern("foo", [], CType::Int)
}

fn bar() -> Stmt<'static, Type, (), LexExtension<'static>> {
    Stmt::xtern("bar", [CType::U32], CType::Void)
}

fn call_foo() -> Stmt<'static, Type, (), LexExtension<'static>> {
    Stmt::function(Function::new(
        "main",
        [],
        [],
        Expr::block(
            [
                Expr::ext(LexExtension::Ident("foo"), ()),
                Expr::ext(LexExtension::Ident("hs_putlnu"), ()),
                Expr::literal(5u32, ()),
                Expr::ext(LexExtension::Ident("bar"), ()),
            ],
            (),
        ),
    ))
}

fn hs_puts() -> Stmt<'static, Type, (), LexExtension<'static>> {
    Stmt::function(Function::new(
        "hs_puts",
        [Type::string()],
        [],
        Expr::block(
            [
                Expr::literal("%.*s", ()),
                Expr::ext(LexExtension::As(vec!["s", "fmt"]), ()),
                Expr::ext(LexExtension::IdentSeq(vec!["fmt", "data"]), ()),
                Expr::ext(LexExtension::IdentSeq(vec!["s", "size"]), ()),
                Expr::ext(LexExtension::IdentSeq(vec!["s", "data"]), ()),
                Expr::ext(LexExtension::Ident("printf"), ()),
                Expr::ext(LexExtension::Ident("drop"), ()),
            ],
            (),
        ),
    ))
}

fn hs_putlns() -> Stmt<'static, Type, (), LexExtension<'static>> {
    Stmt::function(Function::new(
        "hs_putlns",
        [Type::string()],
        [],
        Expr::block(
            [
                Expr::ext(LexExtension::Ident("hs_puts"), ()),
                Expr::literal("\\n", ()),
                Expr::ext(LexExtension::Ident("hs_puts"), ()),
            ],
            (),
        ),
    ))
}

fn hs_putu() -> Stmt<'static, Type, (), LexExtension<'static>> {
    Stmt::function(Function::new(
        "hs_putu",
        [Type::U32],
        [],
        Expr::block(
            [
                Expr::literal("%u", ()),
                Expr::ext(LexExtension::As(vec!["n", "fmt"]), ()),
                Expr::ext(LexExtension::IdentSeq(vec!["fmt", "data"]), ()),
                Expr::ext(LexExtension::Ident("n"), ()),
                Expr::ext(LexExtension::Ident("printf"), ()),
                Expr::ext(LexExtension::Ident("drop"), ()),
            ],
            (),
        ),
    ))
}

fn hs_putlnu() -> Stmt<'static, Type, (), LexExtension<'static>> {
    Stmt::function(Function::new(
        "hs_putlnu",
        [Type::U32],
        [],
        Expr::block(
            [
                Expr::ext(LexExtension::Ident("hs_putu"), ()),
                Expr::literal("\\n", ()),
                Expr::ext(LexExtension::Ident("hs_puts"), ()),
            ],
            (),
        ),
    ))
}

fn main() {
    let mut inference = TypeInference::new();

    let stmts = vec![
        Stmt::strukt(
            "Str",
            [("size", Type::U32), ("data", Type::pointer(Type::U8))],
        ),
        // example(),
        fib(),
        dup(&mut inference),
        haystack_drop(&mut inference),
        printf(),
        foo(),
        bar(),
        call_foo(),
        hs_puts(),
        hs_putlns(),
        hs_putu(),
        hs_putlnu(),
    ];
    let mut extern_fns = HashMap::new();
    let mut extern_names = HashMap::new();
    let mut env = HashMap::new();
    let mut fn_names = HashMap::new();
    let mut fn_idents = HashMap::new();
    let (_types, fns) = stmts.into_iter().fold(
        (HashMap::new(), Vec::new()),
        |(mut types, mut fns), stmt| {
            match stmt {
                Stmt::Function(func) => fns.push(func),
                Stmt::Type(ty) => {
                    if types.insert(ty.name.to_owned(), ty).is_some() {
                        todo!("name conflict error")
                    }
                }
                Stmt::Extern(func) => {
                    let id = extern_fns.len();

                    let name = func.expect_function().name.clone();

                    assert!(extern_names.insert(name, id).is_none());
                    assert!(extern_fns.insert(id, func).is_none());
                }
            }

            (types, fns)
        },
    );

    fns.iter().for_each(|func| {
        let idx = env.len();
        assert!(fn_names.insert(idx, format!("{}", &func.name)).is_none());
        assert!(fn_idents.insert(format!("{}", &func.name), idx).is_none());
        assert!(env.insert(Var::Func(idx), Type::from(func)).is_none());
    });

    // Resolve identifiers into variables & function calls
    let fns = fns
        .into_iter()
        .map(|f| f.resolve_names(&fn_idents, &extern_names));

    // Type check all functions.
    let mut fns = fns
        .map(|f| f.type_check(&mut inference, &env, &extern_fns))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|(func, e)| format!("`{func}` failed to type check: {e:?}"))
        .unwrap();

    // Monomorphize generic function calls
    let fns = {
        let to_monomorphize =
            passes::find_monomorphizations(&mut fns, &env, &mut fn_names, &mut fn_idents);
        passes::resolve_monomorphizations(
            to_monomorphize,
            fns,
            &mut env,
            &extern_fns,
            &fn_names,
            &mut inference,
        )
    };

    // Transpile to c
    let fns = fns
        .into_iter()
        .map(|func| func.into_ssa_form(&env, &extern_fns, &fn_names))
        .flatten()
        .collect::<Vec<_>>();

    generate!(0, "#include <stdio.h>");
    generate!(0, "#include <stdint.h>");
    generate!(0, "#include <stdbool.h>");
    generate!(0, "");

    let mut types = HashMap::from([("Str".to_owned(), CType::from(Type::string()))]);
    fns.iter().for_each(|f| {
        eprintln!("{}", f.name);
        dbg!(&types);
        let new_tys = f.body.get_ctypes();
        dbg!(&new_tys);
        types.extend(new_tys)
    });

    let types = CType::organize_dependencies(types.into_iter().map(|(_, v)| v)).unwrap();

    let indentation = 0;
    let tab_size = 4;
    types.iter().for_each(|ty| ty.declare(indentation));
    generate!(indentation, "");
    types
        .iter()
        .for_each(|ty| ty.transpile(indentation, tab_size));
    generate!(indentation, "");
    extern_fns
        .values()
        .for_each(|decl| decl.transpile(indentation, tab_size));
    generate!(indentation, "");
    fns.iter().for_each(|f| f.declare(indentation, tab_size));
    fns.iter().for_each(|f| f.transpile(indentation, tab_size));
}
