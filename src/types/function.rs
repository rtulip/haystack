use std::{fmt::Display, ops::Sub};

use crate::{ast::arg::TypedArg, error::HayError, lex::token::Token};

use super::{Frame, Stack, Substitutions, Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub input: Vec<Type>,
    pub output: Vec<Type>,
}

impl FunctionType {
    pub fn new(input: Vec<Type>, output: Vec<Type>) -> Self {
        Self { input, output }
    }

    pub fn from_typed_args(inputs: &Vec<TypedArg>, outputs: &Vec<TypedArg>) -> Self {
        Self {
            input: inputs.iter().map(|arg| arg.typ.clone()).collect(),
            output: outputs.iter().map(|arg| arg.typ.clone()).collect(),
        }
    }

    pub fn unify(&self, token: &Token, stack: &mut Stack) -> Result<(), HayError> {
        if stack.len() < self.input.len() {
            println!("{token}",);
            println!("{stack:?}");
            println!("{:?}", self.input);
            todo!()
        }

        let mut s: Vec<_> = (0..self.input.len())
            .into_iter()
            .map(|_| stack.pop().unwrap())
            .collect();

        s.reverse();
        let mut subs = Substitutions::empty();
        for (s, i) in s.iter().zip(self.input.iter()) {
            s.unify(token, i, &mut subs)?;
        }

        stack.extend(self.output.clone().into_iter());
        subs.apply(stack);

        Ok(())
    }

    pub fn unify_many(fns: &[Self], token: &Token, stack: &mut Stack) -> Result<(), HayError> {
        // Make sure that each signature has the same "shape"
        // This might not be strctly nessisary.
        let in_len = fns[0].input.len();
        let out_len = fns[0].output.len();
        if !fns
            .iter()
            .all(|sig| sig.input.len() == in_len && sig.output.len() == out_len)
        {
            let mut e = HayError::new(
                "Logic Error - All signatures should have the same input and output lengths for evaluate many.",
                token.loc.clone(),
            ).with_hint("Found these signatures:");

            for func in fns {
                e = e.with_hint(format!("{func:?}",));
            }
            return Err(e);
        }
        let stack_before = stack.clone();
        for func in fns {
            if let Ok(_) = func.unify(token, stack) {
                return Ok(());
            }

            *stack = stack_before.clone();
        }

        todo!()
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
