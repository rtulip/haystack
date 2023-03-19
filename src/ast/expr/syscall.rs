use crate::{
    error::HayError,
    lex::token::Token,
    types::{Signature, Stack, Type, TypeMap, Variance},
};

use super::TypedExpr;

#[derive(Debug, Clone)]
pub struct ExprSyscall {
    pub n: usize,
    pub token: Token,
}

impl ExprSyscall {
    pub fn type_check(self, stack: &mut Stack, types: &mut TypeMap) -> Result<TypedExpr, HayError> {
        if stack.len() < self.n + 1 {
            return Err(HayError::new_type_err(
                format!(
                    "{} requires at least {} elements on the stack. Found {}",
                    self.token.lexeme,
                    self.n + 1,
                    stack.len()
                ),
                self.token.loc,
            ));
        }

        Signature::new(vec![Type::U64.id()], vec![]).evaluate(
            &self.token,
            stack,
            types,
            Variance::Variant,
        )?;

        for _ in 0..self.n {
            let t = stack.pop().unwrap();
            let size = t.size(types)?;
            if size != 1 {
                return Err(HayError::new(
                    format!("`{}` can only accept types of size 1.", self.token.lexeme),
                    self.token.loc,
                )
                .with_hint(format!("Found type `{t}` which has size {size}")));
            }
        }

        stack.push(Type::U64.id());

        Ok(TypedExpr::Syscall { n: self.n })
    }
}
