use super::{FnTy, Substitution, TyGen, TyVar};

#[derive(Debug, Clone)]
pub struct Scheme<'src> {
    free: Vec<TyVar>,
    func: FnTy<'src>,
}

impl<'src> Scheme<'src> {
    pub fn instantiate(&self, gen: &mut TyGen) -> (FnTy<'src>, Substitution<'src>) {
        let subs = Substitution::from_iter(self.free.iter().map(|t| (*t, gen.fresh())));
        (self.func.clone().substitute(&subs), subs)
    }
}

impl<'src> Scheme<'src> {
    pub fn new<const N: usize>(free: [TyVar; N], func: FnTy<'src>) -> Self {
        Self {
            free: free.into_iter().collect(),
            func,
        }
    }
}
