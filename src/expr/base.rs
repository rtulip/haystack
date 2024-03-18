use crate::expr::Literal;

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
pub enum ExprBase<'src, M, E> {
    Literal(Literal<'src>),
    Print,
    Block(Vec<Expr<'src, M, E>>),
    Ext(E),
}

/// Expressions
///
/// An expression is a representation of different units of code. These can
/// be attached with some metadata `M`, such as sorce location information,
/// types information, etc...
///
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

    pub fn ext<Ext>(ext: Ext, meta: M) -> Self
    where
        Ext: Into<E>,
    {
        Expr {
            expr: ExprBase::<M, E>::Ext(ext.into()),
            meta,
        }
    }
}
