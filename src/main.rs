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
use lex::LexType;
use stmt::{Function, Stmt};

use crate::{
    lex::LexExtension,
    stmt::TypeDeclaration,
    types::{Type, TypeInference, Var},
};

fn fib() -> Stmt<'static, LexType<'static>, (), LexExtension<'static>> {
    Stmt::function(Function::new(
        "fib",
        [LexType::Ident("u32")],
        [LexType::Ident("u32")],
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

fn dup(ctx: &mut TypeInference) -> Stmt<'static, LexType<'static>, (), LexExtension<'static>> {
    Stmt::function(Function::new_generic(
        "dup",
        [LexType::Ident("T")],
        [LexType::Ident("T"), LexType::Ident("T")],
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

fn haystack_drop(
    ctx: &mut TypeInference,
) -> Stmt<'static, LexType<'static>, (), LexExtension<'static>> {
    Stmt::function(Function::new_generic(
        "drop",
        [LexType::Ident("T")],
        [],
        Expr::ext(LexExtension::As(vec!["_"]), ()),
        ["T"],
        ctx,
    ))
}

fn printf() -> Stmt<'static, LexType<'static>, (), LexExtension<'static>> {
    Stmt::xtern(
        "printf",
        [CType::pointer(CType::Char), CType::Variadic],
        CType::Int,
    )
}

fn foo() -> Stmt<'static, LexType<'static>, (), LexExtension<'static>> {
    Stmt::xtern("foo", [], CType::Int)
}

fn bar() -> Stmt<'static, LexType<'static>, (), LexExtension<'static>> {
    Stmt::xtern("bar", [CType::U32], CType::Void)
}

fn call_foo() -> Stmt<'static, LexType<'static>, (), LexExtension<'static>> {
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

fn hs_puts() -> Stmt<'static, LexType<'static>, (), LexExtension<'static>> {
    Stmt::function(Function::new(
        "hs_puts",
        [LexType::Ident("Str")],
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

fn hs_putlns() -> Stmt<'static, LexType<'static>, (), LexExtension<'static>> {
    Stmt::function(Function::new(
        "hs_putlns",
        [LexType::Ident("Str")],
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

fn hs_putu() -> Stmt<'static, LexType<'static>, (), LexExtension<'static>> {
    Stmt::function(Function::new(
        "hs_putu",
        [LexType::Ident("u32")],
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

fn hs_putlnu() -> Stmt<'static, LexType<'static>, (), LexExtension<'static>> {
    Stmt::function(Function::new(
        "hs_putlnu",
        [LexType::Ident("u32")],
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

    let stmts: Vec<Stmt<'_, LexType<'_>, _, LexExtension<'_>>> = vec![
        Stmt::strukt(
            "Str",
            [
                ("size", LexType::Ident("u32")),
                ("data", LexType::pointer(LexType::Ident("u8"))),
            ],
        ),
        Stmt::generic_strukt(
            "Pair",
            [
                ("left", LexType::Ident("T")),
                ("right", LexType::Ident("T")),
            ],
            ["T"],
            &mut inference,
        ),
        Stmt::enumm("Foo", ["Bar", "Baz"]),
        Stmt::tagged_union(
            "Option_U32",
            [
                ("None", LexType::tuple([])),
                ("Some", LexType::Ident("u32")),
            ],
        ),
        fib(),
        dup(&mut inference),
        haystack_drop(&mut inference),
        printf(),
        // foo(),
        // bar(),
        // call_foo(),
        hs_puts(),
        hs_putlns(),
        hs_putu(),
        hs_putlnu(),
        Stmt::Function(Function::new(
            "main",
            [],
            [],
            Expr::block(
                [
                    Expr::literal("Left Member", ()),
                    Expr::literal("Right Member", ()),
                    Expr::cast(LexType::Ident("Pair"), ()),
                    Expr::ext(LexExtension::As(vec!["pair"]), ()),
                    Expr::ext(LexExtension::IdentSeq(vec!["pair", "left"]), ()),
                    Expr::ext(LexExtension::Ident("hs_putlns"), ()),
                    Expr::ext(LexExtension::IdentSeq(vec!["pair", "right"]), ()),
                    Expr::ext(LexExtension::Ident("hs_putlns"), ()),
                    Expr::literal(12345u32, ()),
                    Expr::literal(54321u32, ()),
                    Expr::cast(LexType::Ident("Pair"), ()),
                    Expr::ext(LexExtension::As(vec!["pair"]), ()),
                    Expr::ext(LexExtension::IdentSeq(vec!["pair", "left"]), ()),
                    Expr::ext(LexExtension::Ident("hs_putlnu"), ()),
                    Expr::ext(LexExtension::IdentSeq(vec!["pair", "right"]), ()),
                    Expr::ext(LexExtension::Ident("hs_putlnu"), ()),
                ],
                (),
            ),
        )),
        Stmt::Function(Function::new(
            "OptionSome",
            [LexType::Ident("u32")],
            [LexType::Ident("Option_U32")],
            Expr::cast(
                LexType::variant(LexType::Ident("Option_U32"), LexType::Ident("Some")),
                (),
            ),
        )),
    ];
    let mut extern_fns = HashMap::new();
    let mut extern_names = HashMap::new();
    let mut env = HashMap::new();
    let mut fn_names = HashMap::new();
    let mut fn_idents = HashMap::new();
    let (type_decls, fns) =
        stmts
            .into_iter()
            .fold((Vec::new(), Vec::new()), |(mut types, mut fns), stmt| {
                match stmt {
                    Stmt::Function(func) => fns.push(func),
                    Stmt::Type(ty) => types.push(ty),
                    Stmt::Extern(func) => {
                        let id = extern_fns.len();

                        let name = func.expect_function().name.clone();

                        assert!(extern_names.insert(name, id).is_none());
                        assert!(extern_fns.insert(id, func).is_none());
                    }
                }

                (types, fns)
            });

    let types = TypeDeclaration::to_concrete(type_decls);

    dbg!(&types);

    let fns = fns
        .into_iter()
        .map(|f| f.to_concrete_typed(&types))
        .collect::<Vec<_>>();

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
    let mut fns = fns
        .into_iter()
        .map(|func| func.into_ssa_form(&env, &extern_fns, &fn_names))
        .flatten()
        .collect::<Vec<_>>();

    generate!(0, "#include <stdio.h>");
    generate!(0, "#include <stdint.h>");
    generate!(0, "#include <stdbool.h>");
    generate!(0, "");

    let mut c_types = HashMap::new();
    fns.iter_mut().for_each(|f| {
        let new_tys = f.body.get_ctypes(&types);
        c_types.extend(new_tys)
    });

    c_types.extend(
        types
            .into_iter()
            .map(|(id, (ty, _))| (id.to_string(), ty))
            .filter(|(_, t)| !t.is_free())
            .map(|(id, ty)| (id, CType::from(ty))),
    );

    let types = CType::organize_dependencies(c_types.into_iter().map(|(_, v)| v)).unwrap();

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
