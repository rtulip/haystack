use crate::{
    ast::expr::{TypedExpr, TypedSyscallExpr},
    error::HayError,
    lex::token::Token,
    types::{FunctionType, Stack, Type},
};

#[derive(Debug, Clone)]
pub struct SyscallExpr {
    pub n: usize,
    pub token: Token,
}

impl SyscallExpr {
    pub fn type_check(&self, stack: &mut Stack) -> Result<TypedExpr, HayError> {
        if stack.len() < self.n + 1 {
            return Err(HayError::new_type_err(
                format!(
                    "{} requires at least {} elements on the stack. Found {}",
                    self.token.lexeme,
                    self.n + 1,
                    stack.len()
                ),
                self.token.loc.clone(),
            ));
        }

        FunctionType::new(vec![Type::u64()], vec![]).unify(&self.token, stack)?;

        for _ in 0..self.n {
            let t = stack.pop().unwrap();
            let size = t.size(&self.token)?;
            if size != 1 {
                return Err(HayError::new(
                    format!("`{}` can only accept types of size 1.", self.token.lexeme),
                    self.token.loc.clone(),
                )
                .with_hint(format!("Found type `{t}` which has size {size}")));
            }
        }

        stack.push(Type::u64());

        Ok(TypedExpr::Syscall(TypedSyscallExpr { n: self.n }))
    }
}
