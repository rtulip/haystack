use super::{FnTy, TyVar};

struct Scheme<'a> {
    free: Vec<TyVar>,
    func: FnTy<'a>,
}

impl<'a> Scheme<'a> {
    fn new<T, const N: usize>(free: [T; N], func: FnTy<'a>) -> Self
    where
        T: Into<TyVar>,
    {
        Self {
            free: free.into_iter().map(|n| n.into()).collect(),
            func,
        }
    }
}
