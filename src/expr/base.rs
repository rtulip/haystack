use crate::{expr::Literal, types::Var};

use super::BinOp;

/// Expression Base
///
/// This is an extensible representation of the base expressions Haystack
/// supports. This includes some basic stack operations, like pushing
/// literals to the stack, calling functions, etc..
///
/// The `'src` lifetime represents the lifetime that the source code remains
/// in memory. This is needed for string literals so that they can be cheap
/// to copy
///
/// The `M` generic represents metadata which is associated with a given
/// instance of `Expr`. This allows for extra data to be tacked onto the
/// expressions
///
/// The `E` generic is any user extensions which can be added. This allows for
/// custom operations to be representable within the same structure. This will
/// make it easier to reuse the same base.
#[derive(Debug, Clone)]
pub enum ExprBase<'src, M, E> {
    Literal(Literal<'src>),
    Print,
    PrintString,
    Block(Vec<Expr<'src, M, E>>),
    BinOp(BinOp),
    Call(Var),
    If {
        then: Box<Expr<'src, M, E>>,
        otherwise: Box<Expr<'src, M, E>>,
    },
    While {
        condition: Vec<Expr<'src, M, E>>,
        body: Box<Expr<'src, M, E>>,
    },
    Bind(usize),
    Var(Var),
    Ext(E),
    ExternCall(usize),
}

/// Expressions
///
/// An expression is a representation of different units of code. These can
/// be attached with some metadata `M`, such as sorce location information,
/// types information, etc...
///
#[derive(Debug, Clone)]
pub struct Expr<'src, Meta, Ext> {
    pub expr: ExprBase<'src, Meta, Ext>,
    pub meta: Meta,
}

impl<'src, M, E> Expr<'src, M, E> {
    pub fn new(meta: M, expr: ExprBase<'src, M, E>) -> Self {
        Expr { expr, meta }
    }

    pub fn literal<L>(lit: L, meta: M) -> Self
    where
        L: Into<Literal<'src>>,
    {
        Expr {
            expr: ExprBase::<M, E>::Literal(lit.into()),
            meta,
        }
    }

    pub fn block<B>(block: B, meta: M) -> Self
    where
        B: Into<Vec<Self>>,
    {
        Expr {
            expr: ExprBase::<M, E>::Block(block.into()),
            meta,
        }
    }

    pub fn print(meta: M) -> Self {
        Expr {
            expr: ExprBase::<M, E>::Print,
            meta,
        }
    }

    pub fn print_string(meta: M) -> Self {
        Expr {
            expr: ExprBase::<M, E>::PrintString,
            meta,
        }
    }

    pub fn ext<Ext>(ext: Ext, meta: M) -> Self
    where
        Ext: Into<E>,
    {
        Expr {
            expr: ExprBase::<M, E>::Ext(ext.into()),
            meta,
        }
    }

    pub fn binop<Op>(op: Op, meta: M) -> Self
    where
        Op: Into<BinOp>,
    {
        Expr {
            expr: ExprBase::BinOp(op.into()),
            meta,
        }
    }

    pub fn call(func: Var, meta: M) -> Self {
        assert!(matches!(func, Var::Func(_)));

        Expr {
            expr: ExprBase::Call(func),
            meta,
        }
    }

    pub fn iff(then: Self, otherwise: Self, meta: M) -> Self {
        Expr {
            expr: ExprBase::If {
                then: Box::new(then),
                otherwise: Box::new(otherwise),
            },
            meta,
        }
    }

    pub fn bind(n: usize, meta: M) -> Self {
        Self {
            expr: ExprBase::Bind(n),
            meta,
        }
    }

    pub fn var(var: Var, meta: M) -> Self {
        assert!(matches!(var, Var::Ident(_)));
        Self {
            expr: ExprBase::Var(var),
            meta,
        }
    }

    pub fn wile<C>(condition: C, body: Self, meta: M) -> Self
    where
        C: Into<Vec<Self>>,
    {
        Expr {
            expr: ExprBase::While {
                condition: condition.into(),
                body: Box::new(body),
            },
            meta,
        }
    }

    pub fn xtern(id: usize, meta: M) -> Self {
        Expr {
            expr: ExprBase::ExternCall(id),
            meta,
        }
    }

    pub fn for_each(&self, mut func: impl FnMut(&Self) -> ()) {
        fn helper<'a, M, E>(e: &Expr<'a, M, E>, f: &mut impl FnMut(&Expr<'a, M, E>) -> ()) {
            f(&e);
            match &e.expr {
                ExprBase::Block(exprs) => {
                    exprs.iter().for_each(|e| helper(e, f));
                }
                ExprBase::If { then, otherwise } => {
                    helper(then, f);
                    helper(otherwise, f);
                }
                _ => (),
            }
        }

        helper(&self, &mut func);
    }

    pub fn for_each_meta(&self, mut func: impl FnMut(&M) -> ()) {
        fn helper<'a, M, E>(e: &Expr<'a, M, E>, f: &mut impl FnMut(&M) -> ()) {
            f(&e.meta);
            match &e.expr {
                ExprBase::Block(exprs) => {
                    exprs.iter().for_each(|e| helper(e, f));
                }
                ExprBase::If { then, otherwise } => {
                    helper(then, f);
                    helper(otherwise, f);
                }
                _ => (),
            }
        }

        helper(&self, &mut func);
    }

    pub fn map_meta<M2>(self, mut func: impl FnMut(M) -> M2) -> Expr<'src, M2, E> {
        fn helper<'a, M1, M2, E>(
            e: Expr<'a, M1, E>,
            f: &mut impl FnMut(M1) -> M2,
        ) -> Expr<'a, M2, E> {
            match e.expr {
                ExprBase::Literal(lit) => Expr::literal(lit, f(e.meta)),
                ExprBase::Print => Expr::print(f(e.meta)),
                ExprBase::PrintString => Expr::print_string(f(e.meta)),
                ExprBase::Block(exprs) => Expr::block(
                    exprs.into_iter().map(|e| helper(e, f)).collect::<Vec<_>>(),
                    f(e.meta),
                ),
                ExprBase::BinOp(op) => Expr::binop(op, f(e.meta)),
                ExprBase::Call(var) => Expr::call(var, f(e.meta)),
                ExprBase::If { then, otherwise } => {
                    Expr::iff(helper(*then, f), helper(*otherwise, f), f(e.meta))
                }
                ExprBase::Bind(n) => Expr::bind(n, f(e.meta)),
                ExprBase::Var(var) => Expr::var(var, f(e.meta)),
                ExprBase::Ext(ext) => Expr::ext(ext, f(e.meta)),
                ExprBase::While { condition, body } => Expr::wile(
                    condition
                        .into_iter()
                        .map(|e| helper(e, f))
                        .collect::<Vec<_>>(),
                    helper(*body, f),
                    f(e.meta),
                ),
                crate::expr::ExprBase::ExternCall(id) => todo!(),
            }
        }

        helper(self, &mut func)
    }
}
