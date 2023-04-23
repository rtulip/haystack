use crate::ast::stmt::StmtKind;
use crate::error::HayError;
use crate::lex::token::{Literal, Operator, Token};
use std::collections::HashMap;

use super::{
    AccessorExpr, AnnotatedCallExpr, AsExpr, BlockExpr, ExprCast, ExprIdent, ExprIf, ExprLiteral,
    ExprOperator, ExprReturn, ExprSizeOf, ExprSyscall, ExprUnary, ExprVar, ExprWhile, MatchExpr,
    NeverExpr, TupleExpr, UnpackExpr,
};

/// Haystack's Expression Representation
///
/// Every line of code in haystack that is not a top-level statement is an Expression.
///
/// During compilation, each `Expr` will be converted into a `TypedExpr`, which carries
/// over type information which is needed for code generation.
///
/// Here's a summary of the different kinds of Expressions:
/// * [`Expr::Literal`] represents literal values, such as integers, booleans, and strings
/// * [`Expr::Operator`] represents the different operators, such as `+`, `-`, `@` and `!
/// * [`Expr::Unary`] represents unary operations, such as the address of operators `&` and `*`
/// * [`Expr::Syscall`] represents syscall operations
/// * [`Expr::Cast`] is for casting to different types
/// * [`Expr::Ident`] represents words and identifiers, be it functions, vars, or framed values
/// * [`Expr::Accessor`] is similar to ident, but is used to get at inner members of framed values or types
/// * [`Expr::If`] is a recursively defined Expression for branching
/// * [`Expr::As`] represents the action of binding values to be framed
/// * [`Expr::Var`] represents creating vars at a function level
/// * [`Expr::While`] represents a whlie loop including and the associated conditional.
/// * [`Expr::AnnotatedCall`] Similar to [`Expr::Accessor`], but for specifying types to function calls.
/// * [`Expr::SizeOf`] is for taking the size of a type
/// * [`Expr::Return`] returns from a function.
#[derive(Debug, Clone)]
pub enum Expr {
    Literal(ExprLiteral),
    Tuple(TupleExpr),
    Operator(ExprOperator),
    Unary(ExprUnary),
    Syscall(ExprSyscall),
    Cast(ExprCast),
    Ident(ExprIdent),
    Accessor(AccessorExpr),
    If(ExprIf),
    As(AsExpr),
    Var(ExprVar),
    While(ExprWhile),
    AnnotatedCall(AnnotatedCallExpr),
    SizeOf(ExprSizeOf),
    Return(ExprReturn),
    Match(MatchExpr),
    Never(NeverExpr),
    Unpack(UnpackExpr),
    Block(BlockExpr),
}

impl Expr {
    /// Helper function to quickly get the most pertinent token from an [`Expr`]
    pub fn token(&self) -> &Token {
        match self {
            Expr::Literal(ExprLiteral { token, .. })
            | Expr::Tuple(TupleExpr { token, .. })
            | Expr::Operator(ExprOperator { token, .. })
            | Expr::Syscall(ExprSyscall { token, .. })
            | Expr::Cast(ExprCast { token, .. })
            | Expr::Ident(ExprIdent { ident: token, .. })
            | Expr::Accessor(AccessorExpr { token, .. })
            | Expr::If(ExprIf { token, .. })
            | Expr::As(AsExpr { token, .. })
            | Expr::Var(ExprVar { token, .. })
            | Expr::While(ExprWhile { token, .. })
            | Expr::AnnotatedCall(AnnotatedCallExpr { token, .. })
            | Expr::SizeOf(ExprSizeOf { token, .. })
            | Expr::Return(ExprReturn { token })
            | Expr::Match(MatchExpr { token, .. })
            | Expr::Never(NeverExpr { token })
            | Expr::Unpack(UnpackExpr { token })
            | Expr::Block(BlockExpr { open: token, .. })
            | Expr::Unary(ExprUnary {
                op: ExprOperator { token, .. },
                ..
            }) => token,
        }
    }
}
