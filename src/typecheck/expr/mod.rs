use crate::typecheck::typ::{typ::Mutability, type_id::TypeId};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Var(pub usize);

impl Into<Vec<Var>> for Var {
    fn into(self) -> Vec<Var> {
        vec![self]
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MatchPattern {
    Variant(TypeId),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MatchCase<'src, Meta> {
    pub pattern: MatchPattern,
    pub binding: Option<Var>,
    pub expr: Box<Expr<'src, Meta>>,
}

impl<'src, Meta> MatchCase<'src, Meta> {
    pub fn new(pattern: MatchPattern, binding: Option<Var>, expr: Expr<'src, Meta>) -> Self {
        Self {
            pattern,
            binding,
            expr: Box::new(expr),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MatchExpr<'src, Meta> {
    pub cases: Vec<MatchCase<'src, Meta>>,
    pub finally: Option<Box<Expr<'src, Meta>>>,
}

impl<'src, Meta> MatchExpr<'src, Meta> {
    pub fn new<Cases>(cases: Cases, finally: Option<Expr<'src, Meta>>) -> Self
    where
        Cases: Into<Vec<MatchCase<'src, Meta>>>,
    {
        Self {
            cases: cases.into(),
            finally: finally.map(|e| Box::new(e)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExprKind<'src, Meta> {
    U8(u8),
    U64(u64),
    Bool(bool),
    String(&'src str),
    Ident(Var),
    Ref { var: Var, mutability: Mutability },
    Block(Vec<Expr<'src, Meta>>),
    Tuple(Vec<Expr<'src, Meta>>),
    Bind(Vec<Var>),
    Call,
    Type(TypeId),
    UncheckedCast,
    Pack,
    MemberAccess { var: Var, member: usize },
    Match(MatchExpr<'src, Meta>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Expr<'src, Meta> {
    pub expr: ExprKind<'src, Meta>,
    pub meta: Meta,
}

impl<'src, Meta> Expr<'src, Meta> {
    pub fn u8(val: u8, meta: Meta) -> Expr<'src, Meta> {
        Expr {
            expr: ExprKind::U8(val),
            meta,
        }
    }

    pub fn u64(val: u64, meta: Meta) -> Expr<'src, Meta> {
        Expr {
            expr: ExprKind::U64(val),
            meta,
        }
    }

    pub fn bool(b: bool, meta: Meta) -> Expr<'src, Meta> {
        Expr {
            expr: ExprKind::Bool(b),
            meta,
        }
    }

    pub fn string(s: &'src str, meta: Meta) -> Expr<'src, Meta> {
        Expr {
            expr: ExprKind::String(s),
            meta,
        }
    }

    pub fn ident(var: Var, meta: Meta) -> Expr<'src, Meta> {
        Expr {
            expr: ExprKind::Ident(var),
            meta,
        }
    }

    pub fn block<Exprs>(exprs: Exprs, meta: Meta) -> Expr<'src, Meta>
    where
        Exprs: Into<Vec<Expr<'src, Meta>>>,
    {
        Expr {
            expr: ExprKind::Block(exprs.into()),
            meta,
        }
    }

    pub fn tuple<Exprs>(exprs: Exprs, meta: Meta) -> Expr<'src, Meta>
    where
        Exprs: Into<Vec<Expr<'src, Meta>>>,
    {
        Expr {
            expr: ExprKind::Tuple(exprs.into()),
            meta,
        }
    }

    pub fn bind<Vars>(vars: Vars, meta: Meta) -> Expr<'src, Meta>
    where
        Vars: Into<Vec<Var>>,
    {
        Expr {
            expr: ExprKind::Bind(vars.into()),
            meta,
        }
    }

    pub fn take_ref(var: Var, mutability: Mutability, meta: Meta) -> Expr<'src, Meta> {
        Expr {
            expr: ExprKind::Ref { var, mutability },
            meta,
        }
    }

    pub fn call(meta: Meta) -> Expr<'src, Meta> {
        Expr {
            expr: ExprKind::Call,
            meta,
        }
    }

    pub fn typ(tid: TypeId, meta: Meta) -> Expr<'src, Meta> {
        Expr {
            expr: ExprKind::Type(tid),
            meta,
        }
    }

    pub fn unchecked_cast(meta: Meta) -> Self {
        Expr {
            expr: ExprKind::UncheckedCast,
            meta,
        }
    }

    pub fn pack(meta: Meta) -> Self {
        Expr {
            expr: ExprKind::Pack,
            meta,
        }
    }

    pub fn member_access(var: Var, member: usize, meta: Meta) -> Self {
        Expr {
            expr: ExprKind::MemberAccess { var, member },
            meta,
        }
    }

    pub fn match_(m: MatchExpr<'src, Meta>, meta: Meta) -> Self {
        Expr {
            expr: ExprKind::Match(m),
            meta,
        }
    }
}

impl<'src, Meta> Into<Vec<Expr<'src, Meta>>> for Expr<'src, Meta> {
    fn into(self) -> Vec<Expr<'src, Meta>> {
        vec![self]
    }
}
