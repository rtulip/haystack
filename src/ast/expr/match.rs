use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        arg::{IdentArg, IdentArgKind},
        expr::{AccessorExpr, AsExpr, ExprElseIf, ExprIf, ExprLiteral, OperatorExpr},
        member::TypedMember,
    },
    error::HayError,
    lex::token::{Keyword, Literal, Operator, Token, TokenKind},
};

use super::{BlockExpr, Expr};

#[derive(Debug, Clone)]
pub struct MatchExpr {
    pub token: Token,
    pub cases: Vec<MatchCaseExpr>,
    pub else_case: Option<MatchElseExpr>,
}

#[derive(Debug, Clone)]
pub struct MatchCaseExpr {
    pub variant: Token,
    pub ident: Option<IdentArg>,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct MatchElseExpr {
    pub token: Token,
    pub body: Box<Expr>,
}

impl MatchExpr {}
