use super::arg::Arg;
use crate::ast::expr::Expr;
use crate::lex::token::Token;
use crate::types::Type;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Visitiliby {
    Public,
    Private,
}

pub struct Member {
    pub vis: Visitiliby,
    pub token: Token,
    pub ident: Token,
    pub typ: Option<Type>,
}

pub enum Stmt {
    Function {
        token: Token,
        name: Token,
        inputs: Vec<Arg>,
        outputs: Vec<Arg>,
        annotations: Option<Vec<Arg>>,
        body: Vec<Box<Expr>>,
    },
    Structure {
        token: Token,
        name: Token,
        annotations: Option<Vec<Arg>>,
        members: Vec<Member>,
        union: bool,
    },
    Enum {
        token: Token,
        name: Token,
        variants: Vec<Token>,
    },
    Var {
        token: Token,
        expr: Box<Expr>,
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
            Stmt::Structure {
                token,
                name,
                members,
                union,
                ..
            } => {
                writeln!(
                    f,
                    "[{}] {} {}:",
                    token.loc,
                    if *union { "struct" } else { "union" },
                    name.lexeme
                )?;
                for mem in members {
                    writeln!(f, "  {}: {}", mem.token.lexeme, mem.ident.lexeme)?;
                }

                Ok(())
            }
            Stmt::Enum {
                token,
                name,
                variants,
            } => {
                writeln!(f, "[{}] enum {}:", token.loc, name.lexeme)?;
                for v in variants {
                    writeln!(f, "  {}", v.lexeme)?;
                }

                Ok(())
            }
            Stmt::Var { token, expr } => {
                write!(f, "[{}] {expr}", token.loc)
            }
        }
    }
}
