use std::fmt::Display;

use crate::ast::arg::TypedArg;

use super::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub input: Vec<Type>,
    pub output: Vec<Type>,
}

impl FunctionType {
    pub fn from_typed_args(inputs: &Vec<TypedArg>, outputs: &Vec<TypedArg>) -> Self {
        Self {
            input: inputs.iter().map(|arg| arg.typ.clone()).collect(),
            output: outputs.iter().map(|arg| arg.typ.clone()).collect(),
        }
    }
}

impl Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        if self.input.len() > 0 {
            for i in &self.input[0..self.input.len() - 1] {
                write!(f, "{i:?} ")?;
            }
        }
        if let Some(i) = self.input.last() {
            write!(f, "{i:?}")?;
        }

        write!(f, ")")?;

        if self.output.len() > 0 {
            write!(f, " -> [")?;

            for o in &self.output[0..self.output.len() - 1] {
                write!(f, "{o:?} ")?;
            }
            if let Some(o) = self.output.last() {
                write!(f, "{o:?}")?;
            }

            write!(f, "]")?;
        }

        Ok(())
    }
}
