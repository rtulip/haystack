use std::fmt::Debug;

use super::{sequence::TySeq, ty::Ty};

pub struct FnTy<'a> {
    input: TySeq<'a>,
    output: TySeq<'a>,
}

impl<'a> FnTy<'a> {
    pub fn new<const IN: usize, const OUT: usize>(
        input: [Ty<'a>; IN],
        output: [Ty<'a>; OUT],
    ) -> Self {
        Self {
            input: input.into(),
            output: output.into(),
        }
    }
}

impl<'a> Debug for FnTy<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}) -> [{}]",
            self.input
                .iter()
                .map(|t| format!("{t:?}"))
                .collect::<Vec<_>>()
                .join(" "),
            self.output
                .iter()
                .map(|t| format!("{t:?}"))
                .collect::<Vec<_>>()
                .join(" ")
        )
    }
}
