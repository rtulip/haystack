use std::fmt::{Debug, Display};

use crate::types::Stack;

use super::types::CType;

#[derive(Clone)]
pub struct CVar {
    pub ty: CType,
    ident: usize,
    member: Option<usize>,
}

impl CVar {
    pub fn new(ty: CType, counter: &mut usize) -> Self {
        let ident = *counter;
        *counter += 1;

        CVar {
            ty,
            ident,
            member: None,
        }
    }

    pub fn from_stack(stack: Stack) -> (Vec<Self>, usize) {
        let mut counter = 0;
        let stack = stack
            .into_iter()
            .map(|t| CVar::new(t.into(), &mut counter))
            .collect();

        return (stack, counter);
    }

    pub fn tuple_member(&self, idx: usize) -> Self {
        match &self.ty {
            CType::Tuple(ts) => {
                assert!(idx < ts.len(), "Out of Bounds tuple access!");

                Self {
                    ty: ts[idx].clone(),
                    ident: self.ident,
                    member: Some(idx),
                }
            }
            ty => panic!("{ty:?} is not a tuple!"),
        }
    }
}

impl<'src> Debug for CVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.ty, self)
    }
}

impl<'src> Display for CVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var{}", self.ident)?;
        if let Some(member) = &self.member {
            write!(f, ".member{member}")?;
        }
        Ok(())
    }
}