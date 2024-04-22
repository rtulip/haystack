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
pub enum ExprBase<'src, M, E, T> {
    Literal(Literal<'src>),
    Block(Vec<Expr<'src, M, E, T>>),
    BinOp(BinOp),
    Call(Var),
    If {
        then: Box<Expr<'src, M, E, T>>,
        otherwise: Box<Expr<'src, M, E, T>>,
    },
    While {
        condition: Vec<Expr<'src, M, E, T>>,
        body: Box<Expr<'src, M, E, T>>,
    },
    Bind(usize),
    Var(Var),
    Ext(E),
    ExternCall(usize),
    VarAccessor(Var, Vec<&'src str>),
    Cast(T),
}

/// Expressions
///
/// An expression is a representation of different units of code. These can
/// be attached with some metadata `M`, such as sorce location information,
/// types information, etc...
///
#[derive(Debug, Clone)]
pub struct Expr<'src, Meta, Ext, Ty> {
    pub expr: ExprBase<'src, Meta, Ext, Ty>,
    pub meta: Meta,
}

impl<'src, M, E, T> Expr<'src, M, E, T> {
    pub fn new(meta: M, expr: ExprBase<'src, M, E, T>) -> Self {
        Expr { expr, meta }
    }

    pub fn literal<L>(lit: L, meta: M) -> Self
    where
        L: Into<Literal<'src>>,
    {
        Expr {
            expr: ExprBase::<M, E, T>::Literal(lit.into()),
            meta,
        }
    }

    pub fn block<B>(block: B, meta: M) -> Self
    where
        B: Into<Vec<Self>>,
    {
        Expr {
            expr: ExprBase::<M, E, T>::Block(block.into()),
            meta,
        }
    }

    pub fn ext<Ext>(ext: Ext, meta: M) -> Self
    where
        Ext: Into<E>,
    {
        Expr {
            expr: ExprBase::<M, E, T>::Ext(ext.into()),
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

    pub fn accessors<Inner>(var: Var, inner: Inner, meta: M) -> Self
    where
        Inner: Into<Vec<&'src str>>,
    {
        let inner = inner.into();
        assert!(inner.len() >= 1);

        Self {
            expr: ExprBase::VarAccessor(var, inner),
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

    pub fn cast(ty: T, meta: M) -> Self {
        Expr {
            expr: ExprBase::Cast(ty),
            meta,
        }
    }

    pub fn for_each(&self, mut func: impl FnMut(&Self) -> ()) {
        fn helper<'a, M, E, T>(
            e: &Expr<'a, M, E, T>,
            f: &mut impl FnMut(&Expr<'a, M, E, T>) -> (),
        ) {
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
        fn helper<'a, M, E, T>(e: &Expr<'a, M, E, T>, f: &mut impl FnMut(&M) -> ()) {
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

    pub fn for_each_meta_mut(&mut self, mut func: impl FnMut(&mut M) -> ()) {
        fn helper<'a, M, E, T>(e: &mut Expr<'a, M, E, T>, f: &mut impl FnMut(&mut M) -> ()) {
            f(&mut e.meta);
            match &mut e.expr {
                ExprBase::Block(exprs) => {
                    exprs.iter_mut().for_each(|e| helper(e, f));
                }
                ExprBase::If { then, otherwise } => {
                    helper(then, f);
                    helper(otherwise, f);
                }
                _ => (),
            }
        }

        helper(self, &mut func);
    }

    pub fn map_meta<M2>(self, mut func: impl FnMut(M) -> M2) -> Expr<'src, M2, E, T> {
        fn helper<'a, M1, M2, E, T>(
            e: Expr<'a, M1, E, T>,
            f: &mut impl FnMut(M1) -> M2,
        ) -> Expr<'a, M2, E, T> {
            match e.expr {
                ExprBase::Literal(lit) => Expr::literal(lit, f(e.meta)),
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
                ExprBase::VarAccessor(var, accessors) => todo!(),
                ExprBase::Ext(ext) => Expr::ext(ext, f(e.meta)),
                ExprBase::While { condition, body } => Expr::wile(
                    condition
                        .into_iter()
                        .map(|e| helper(e, f))
                        .collect::<Vec<_>>(),
                    helper(*body, f),
                    f(e.meta),
                ),
                ExprBase::ExternCall(id) => todo!(),
                ExprBase::Cast(ty) => Expr::cast(ty, f(e.meta)),
            }
        }

        helper(self, &mut func)
    }
}
