use crate::lex::token::Token;

pub enum Expr {
    Literal { value: Token },
    Operator { op: Token },
    Syscall { token: Token },
    Cast { token: Token, typ: Token },
    Ident { ident: Token },
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal { value: token }
            | Expr::Operator { op: token }
            | Expr::Syscall { token }
            | Expr::Cast { token, .. }
            | Expr::Ident { ident: token } => {
                write!(f, "{token}")
            }
        }
    }
}
