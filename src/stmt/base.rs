use super::Function;

pub enum StmtBase<'src, M, E> {
    Function(Function<'src, M, E>),
}

pub struct Stmt<'src, StmtMeta, ExprMeta, ExprExt> {
    stmt: StmtBase<'src, ExprMeta, ExprExt>,
    meta: StmtMeta,
}

impl<'src, M, EM, E> Stmt<'src, M, EM, E> {
    pub fn function<F>(f: F, meta: M) -> Self
    where
        F: Into<Function<'src, EM, E>>,
    {
        Stmt {
            stmt: StmtBase::Function(f.into()),
            meta,
        }
    }
}