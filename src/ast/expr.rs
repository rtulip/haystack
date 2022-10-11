use crate::ast::arg::Arg;
use crate::lex::token::Token;
use crate::types::Type;

#[derive(Debug, Clone)]
pub enum Expr {
    Literal {
        value: Token,
    },
    Operator {
        op: Token,
    },
    Syscall {
        token: Token,
    },
    Cast {
        token: Token,
        typ: Token,
    },
    Ident {
        ident: Token,
    },
    Accessor {
        token: Token,
        ident: Token,
        inner: Vec<Token>,
    },
    If {
        token: Token,
        then: Vec<Box<Expr>>,
        otherwise: Vec<Box<Expr>>,
        finally: Option<Vec<Box<Expr>>>,
    },
    ElseIf {
        else_tok: Token,
        condition: Vec<Box<Expr>>,
        block: Vec<Box<Expr>>,
    },
    As {
        token: Token,
        args: Vec<Arg>,
        block: Option<Vec<Box<Expr>>>,
    },
    Var {
        token: Token,
        typ: Token,
        ident: Token,
    },
    While {
        token: Token,
        cond: Vec<Box<Expr>>,
        body: Vec<Box<Expr>>,
    },
    AnnotatedCall {
        token: Token,
        base: Token,
        annotations: Vec<Arg>,
    },
    SizeOf {
        token: Token,
        typ: Token,
    },
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal { value: token }
            | Expr::Operator { op: token }
            | Expr::Syscall { token }
            | Expr::Cast { token, .. }
            | Expr::Ident { ident: token }
            | Expr::Accessor { token, .. }
            | Expr::If { token, .. }
            | Expr::ElseIf {
                else_tok: token, ..
            }
            | Expr::As { token, .. }
            | Expr::Var { token, .. }
            | Expr::While { token, .. }
            | Expr::SizeOf { token, .. } => {
                write!(f, "{token}")
            }
            Expr::AnnotatedCall {
                base, annotations, ..
            } => {
                write!(f, "{}<", base.lexeme)?;
                for arg in &annotations[0..annotations.len() - 1] {
                    write!(f, "{} ", arg.token.lexeme)?;
                }
                write!(f, "{}>", annotations.last().unwrap().token.lexeme)
            }
        }
    }
}
