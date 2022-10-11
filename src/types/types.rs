use crate::ast::arg::Arg;
use crate::ast::expr::Expr;
use crate::ast::stmt::Member;
use crate::lex::token::Token;

#[derive(Debug, Clone)]
pub enum Type {
    // U8,
    // U64,
    // Char,
    Struct {
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
    Function {
        token: Token,
        name: Token,
        inputs: Vec<Arg>,
        outputs: Vec<Arg>,
        annotations: Option<Vec<Arg>>,
        body: Vec<Box<Expr>>,
    },
}
