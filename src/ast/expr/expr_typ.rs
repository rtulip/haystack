use crate::ast::stmt::{Functions, InterfaceFunctionTable, Interfaces, StmtKind, UserDefinedTypes};
use crate::error::HayError;
use crate::lex::token::{Literal, Operator, Token};
use crate::types::{Frame, Stack, Substitutions};
use std::collections::HashMap;

use super::{
    AccessorExpr, AnnotatedCallExpr, AsExpr, BlockExpr, CastExpr, ExprReturn, ExprSizeOf,
    ExprSyscall, ExprUnary, ExprVar, ExprWhile, IdentExpr, IfExpr, LiteralExpr, MatchExpr,
    NeverExpr, OperatorExpr, TupleExpr, UnpackExpr,
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
    Literal(LiteralExpr),
    Tuple(TupleExpr),
    Operator(OperatorExpr),
    Unary(ExprUnary),
    Syscall(ExprSyscall),
    Cast(CastExpr),
    Ident(IdentExpr),
    Accessor(AccessorExpr),
    If(IfExpr),
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
            Expr::Literal(LiteralExpr { token, .. })
            | Expr::Tuple(TupleExpr { token, .. })
            | Expr::Operator(OperatorExpr { token, .. })
            | Expr::Syscall(ExprSyscall { token, .. })
            | Expr::Cast(CastExpr { token, .. })
            | Expr::Ident(IdentExpr { ident: token, .. })
            | Expr::Accessor(AccessorExpr { token, .. })
            | Expr::If(IfExpr { token, .. })
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
                op: OperatorExpr { token, .. },
                ..
            }) => token,
        }
    }

    pub fn type_check(
        &self,
        types: &UserDefinedTypes,
        stack: &mut Stack,
        frame: &mut Frame,
        functions: &Functions,
        interfaces: &Interfaces,
        interface_fn_table: &InterfaceFunctionTable,
        subs: &mut Substitutions,
    ) -> Result<(), HayError> {
        match self {
            Expr::Block(block) => block.type_check(
                types,
                stack,
                frame,
                functions,
                interfaces,
                interface_fn_table,
                subs,
            ),
            Expr::Ident(ident) => ident.type_check(
                types,
                stack,
                frame,
                functions,
                interfaces,
                interface_fn_table,
            ),
            Expr::Accessor(accessor) => accessor.type_check(types, stack, frame, subs),
            Expr::Operator(operator) => operator.type_check(types, stack, frame, interfaces),
            Expr::If(if_expr) => if_expr.type_check(
                types,
                stack,
                frame,
                functions,
                interfaces,
                interface_fn_table,
                subs,
            ),
            Expr::Literal(literal) => literal.type_check(types, stack),
            Expr::While(while_expr) => while_expr.type_check(
                types,
                stack,
                frame,
                functions,
                interfaces,
                interface_fn_table,
                subs,
            ),
            Expr::As(as_expr) => as_expr.type_check(stack, frame),
            Expr::Cast(cast) => cast.type_check(types, stack),
            x => todo!("{x:?}"),
        }
    }
}
