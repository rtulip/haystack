use super::arg::Arg;
use crate::ast::expr::Expr;
use crate::lex::token::Token;

pub enum Stmt {
    Function {
        token: Token,
        name: Token,
        inputs: Vec<Arg>,
        outputs: Vec<Arg>,
        body: Vec<Expr>,
    },
}

impl std::fmt::Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Function {
                token, name, body, ..
            } => {
                writeln!(f, "[{}] fn {}:", token.loc, name.lexeme)?;
                for e in body {
                    writeln!(f, "  {e}")?;
                }

                Ok(())
            }
        }
    }
}
