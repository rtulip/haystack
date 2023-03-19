use super::{TypeId, TypeMap};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Variance {
    Variant = 0,
    Covariant = 1,
    Contravariant = 2,
    Invariant = 3,
}

impl Variance {
    pub fn new(t1: &TypeId, t2: &TypeId, types: &TypeMap) -> Self {
        if t1 == t2 {
            Variance::Variant
        } else if t1 == &t2.supertype(types) {
            Variance::Contravariant
        } else if &t1.supertype(types) == t2 {
            Variance::Covariant
        } else {
            Variance::Invariant
        }
    }
}
