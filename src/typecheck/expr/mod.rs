use crate::typecheck::typ::{typ::Mutability, type_id::TypeId};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Var(pub usize);

impl Into<Vec<Var>> for Var {
    fn into(self) -> Vec<Var> {
        vec![self]
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum MatchPattern {
    Variant(TypeId),
}

#[derive(Debug, PartialEq, Eq)]
pub struct MatchCase<Meta> {
    pub pattern: MatchPattern,
    pub binding: Option<Var>,
    pub expr: Box<Expr<Meta>>,
}

impl<Meta> MatchCase<Meta> {
    pub fn new(pattern: MatchPattern, binding: Option<Var>, expr: Expr<Meta>) -> Self {
        Self {
            pattern,
            binding,
            expr: Box::new(expr),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct MatchExpr<Meta> {
    pub cases: Vec<MatchCase<Meta>>,
    pub finally: Option<Box<Expr<Meta>>>,
}

impl<Meta> MatchExpr<Meta> {
    pub fn new<Cases>(cases: Cases, finally: Option<Expr<Meta>>) -> Self
    where
        Cases: Into<Vec<MatchCase<Meta>>>,
    {
        Self {
            cases: cases.into(),
            finally: finally.map(|e| Box::new(e)),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind<Meta> {
    U8,
    Bool,
    Ident(Var),
    Ref { var: Var, mutability: Mutability },
    Block(Vec<Expr<Meta>>),
    Bind(Vec<Var>),
    Call,
    Type(TypeId),
    UncheckedCast,
    Pack,
    MemberAccess { var: Var, member: usize },
    Match(MatchExpr<Meta>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Expr<Meta> {
    pub expr: ExprKind<Meta>,
    pub meta: Meta,
}

impl<Meta> Expr<Meta> {
    pub fn u8(meta: Meta) -> Expr<Meta> {
        Expr {
            expr: ExprKind::U8,
            meta,
        }
    }

    pub fn bool(meta: Meta) -> Expr<Meta> {
        Expr {
            expr: ExprKind::Bool,
            meta,
        }
    }

    pub fn ident(var: Var, meta: Meta) -> Expr<Meta> {
        Expr {
            expr: ExprKind::Ident(var),
            meta,
        }
    }

    pub fn block<Exprs>(exprs: Exprs, meta: Meta) -> Expr<Meta>
    where
        Exprs: Into<Vec<Expr<Meta>>>,
    {
        Expr {
            expr: ExprKind::Block(exprs.into()),
            meta,
        }
    }

    pub fn bind<Vars>(vars: Vars, meta: Meta) -> Expr<Meta>
    where
        Vars: Into<Vec<Var>>,
    {
        Expr {
            expr: ExprKind::Bind(vars.into()),
            meta,
        }
    }

    pub fn take_ref(var: Var, mutability: Mutability, meta: Meta) -> Expr<Meta> {
        Expr {
            expr: ExprKind::Ref { var, mutability },
            meta,
        }
    }

    pub fn call(meta: Meta) -> Expr<Meta> {
        Expr {
            expr: ExprKind::Call,
            meta,
        }
    }

    pub fn typ(tid: TypeId, meta: Meta) -> Expr<Meta> {
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

    pub fn match_(m: MatchExpr<Meta>, meta: Meta) -> Self {
        Expr {
            expr: ExprKind::Match(m),
            meta,
        }
    }
}

impl<Meta> Into<Vec<Expr<Meta>>> for Expr<Meta> {
    fn into(self) -> Vec<Expr<Meta>> {
        vec![self]
    }
}
