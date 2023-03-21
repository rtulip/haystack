use crate::ast::stmt::StmtKind;
use crate::error::HayError;
use crate::lex::token::{Literal, Operator, Token};
use crate::types::{Frame, FramedType, Signature, Stack, Type, TypeId, TypeMap, UncheckedFunction};
use std::collections::HashMap;

use super::{
    AccessorExpr, AnnotatedCallExpr, AsExpr, ExprCast, ExprIdent, ExprIf, ExprLiteral,
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
            | Expr::Unary(ExprUnary {
                op: ExprOperator { token, .. },
                ..
            }) => token,
        }
    }

    /// Type checks an expression
    pub fn type_check(
        self,
        stack: &mut Stack,
        frame: &mut Frame,
        func: &UncheckedFunction,
        global_env: &mut HashMap<String, (StmtKind, Signature)>,
        types: &mut TypeMap,
        generic_map: &Option<HashMap<TypeId, TypeId>>,
    ) -> Result<TypedExpr, HayError> {
        // If the stack has a Never type this expression is unreachable.
        // Don't allow unreachable expressions.
        if stack.contains(&Type::Never.id()) {
            return Err(HayError::new(
                "Unreachable expression.",
                self.token().loc.clone(),
            ));
        }

        // Type checking is different for each kind of expression.
        match self {
            Expr::Accessor(e) => e.type_check(stack, frame, func, types),
            Expr::AnnotatedCall(e) => e.type_check(stack, global_env, types, generic_map),
            Expr::As(e) => e.type_check(stack, frame, func, global_env, types, generic_map),
            Expr::Cast(e) => e.type_check(stack, types, func, generic_map),
            Expr::Ident(e) => e.type_check(stack, frame, types, global_env),
            Expr::If(e) => e.type_check(stack, frame, func, global_env, types, generic_map),
            Expr::Literal(e) => e.type_check(stack),
            Expr::Tuple(e) => e.type_check(stack, frame, func, global_env, types, generic_map),
            Expr::Unary(e) => e.type_check(stack, frame, types, func),
            Expr::Operator(e) => e.type_check(stack, types, global_env),
            Expr::SizeOf(e) => e.type_check(stack, types, generic_map),
            Expr::Syscall(e) => e.type_check(stack, types),
            Expr::Var(e) => e.type_check(frame, types),
            Expr::While(e) => e.type_check(stack, frame, func, global_env, types, generic_map),
            Expr::Return(e) => e.type_check(stack, func),
            Expr::Match(e) => e.type_check(stack, frame, func, global_env, types, generic_map),
            Expr::Never(e) => e.type_check(stack),
            Expr::Unpack(e) => e.type_check(stack, types),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypedExpr {
    Literal {
        value: Literal,
    },
    Operator {
        op: Operator,
        typ: Option<TypeId>,
    },
    Syscall {
        n: usize,
    },
    Cast {
        typ: TypeId,
    },
    Unpack {
        typ: TypeId,
    },
    CastEnumStruct {
        padding: usize,
        idx: usize,
    },
    If {
        then: Vec<TypedExpr>,
        otherwise: Vec<TypedExpr>,
        finally: Option<Vec<TypedExpr>>,
    },
    ElseIf {
        condition: Vec<TypedExpr>,
        block: Vec<TypedExpr>,
    },
    As {
        args: Vec<TypeId>,
        block: Option<Vec<TypedExpr>>,
    },
    Var {
        size: usize,
        width: usize,
        data: Option<(usize, usize)>,
    },
    While {
        cond: Vec<TypedExpr>,
        body: Vec<TypedExpr>,
    },
    Call {
        func: String,
    },
    Framed {
        frame: Vec<(String, FramedType)>,
        idx: usize,
        inner: Option<Vec<String>>,
    },
    AddrFramed {
        frame: Vec<(String, FramedType)>,
        idx: usize,
        inner: Option<Vec<String>>,
    },
    FramedPointerOffset {
        frame: Vec<(String, FramedType)>,
        idx: usize,
        inner: Vec<String>,
    },
    Enum {
        typ: TypeId,
        variant: String,
    },
    Tuple {
        exprs: Vec<TypedExpr>,
    },
    Global {
        ident: String,
    },
    Pad {
        padding: usize,
    },
    Return,
}
