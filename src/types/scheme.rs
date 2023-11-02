use super::{FnTy, Substitution, TyGen, TyVar};

#[derive(Debug, Clone)]
pub struct Scheme<'src> {
    free: Vec<TyVar>,
    func: FnTy<'src>,
}

impl<'src> Scheme<'src> {
    pub fn substitute(self, subs: &Substitution<'src>) -> Self {
        let subs = subs
            .clone()
            .into_iter()
            .filter(|(k, _)| !self.free.contains(k))
            .collect();

        Self {
            free: self.free,
            func: self.func.substitute(&subs),
        }
    }

    pub fn instantiate(&self, gen: &mut TyGen) -> FnTy<'src> {
        let subs = Substitution::from_iter(self.free.iter().map(|t| (*t, gen.fresh())));
        self.func.clone().substitute(&subs)
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
