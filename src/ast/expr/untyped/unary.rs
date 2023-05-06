use crate::{
    ast::expr::TypedExpr,
    error::HayError,
    lex::token::{Keyword, Operator},
    types::{Frame, PointerType, Stack, Type},
};

use super::{AccessorExpr, Expr, IdentExpr, OperatorExpr};

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: OperatorExpr,
    pub expr: Box<Expr>,
}

impl UnaryExpr {
    pub fn type_check(&self, stack: &mut Stack, frame: &Frame) -> Result<TypedExpr, HayError> {
        match &self.op.op {
            Operator::Ampersand => match &self.expr {
                box Expr::Ident(IdentExpr { ident }) => {
                    if let Some((_, t)) = frame.iter().find(|(s, _)| s == &ident.lexeme) {
                        stack.push(Type::Pointer(PointerType {
                            mutable: false,
                            inner: Box::new(t.clone()),
                        }));
                    } else {
                        todo!()
                    }
                }
                box Expr::Accessor(AccessorExpr {
                    ident,
                    inner,
                    token,
                }) => {
                    if let Some((_, t)) = frame.iter().find(|(s, _)| s == &ident.lexeme) {
                        stack.push(Type::Pointer(PointerType {
                            mutable: false,
                            inner: Box::new(t.get_inner_accessors(&token, &inner)?),
                        }));
                    } else {
                        todo!()
                    }
                }
                x => todo!("{}: {x:?}", x.token()),
            },
            Operator::Star => match &self.expr {
                box Expr::Ident(ident) => {
                    if let Some((_, t)) = frame.iter().find(|(s, _)| s == &ident.ident.lexeme) {
                        stack.push(Type::Pointer(PointerType {
                            mutable: true,
                            inner: Box::new(t.clone()),
                        }));
                    } else {
                        todo!()
                    }
                }
                box Expr::Accessor(AccessorExpr {
                    ident,
                    inner,
                    token,
                }) => {
                    if let Some((_, t)) = frame.iter().find(|(s, _)| s == &ident.lexeme) {
                        stack.push(Type::Pointer(PointerType {
                            mutable: true,
                            inner: Box::new(t.get_inner_accessors(&token, &inner)?),
                        }));
                    } else {
                        todo!()
                    }
                }
                e => todo!("{e:?}"),
            },
            op => todo!("{op:?}"),
        }

        Ok(todo!())
    }
}
