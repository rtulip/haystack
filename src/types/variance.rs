use super::{Type, TypeId, TypeMap};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Variance {
    Variant = 0,
    Covariant = 1,
    Contravariant = 2,
    Invariant = 3,
}

impl Variance {
    pub fn new(t1: &TypeId, t2: &TypeId, types: &mut TypeMap) -> Self {
        let mut insert = vec![];
        let t1 = match types.get(t1) {
            Some(Type::Tuple {
                inner,
                idents: Some(_),
            }) => {
                let t = Type::Tuple {
                    inner: inner.clone(),
                    idents: None,
                };
                let tid = t.id();
                insert.push(t);
                tid
            }
            _ => t1.clone(),
        };

        let t2 = match types.get(t2) {
            Some(Type::Tuple {
                inner,
                idents: Some(_),
            }) => {
                let t = Type::Tuple {
                    inner: inner.clone(),
                    idents: None,
                };

                let tid = t.id();
                insert.push(t);
                tid
            }
            _ => t2.clone(),
        };

        for t in insert {
            let id = t.id();
            types.insert(id, t);
        }

        if t1 == t2 {
            Variance::Variant
        } else if t1 == t2.supertype(types) {
            Variance::Contravariant
        } else if t1.supertype(types) == t2 {
            Variance::Covariant
        } else {
            Variance::Invariant
        }
    }
}
