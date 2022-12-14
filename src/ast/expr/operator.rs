use crate::{
    ast::stmt::{GlobalEnv, StmtKind},
    error::HayError,
    lex::token::{Operator, Token},
    types::{Signature, Stack, Type, TypeId, TypeMap},
};

use super::{ExprIdent, TypedExpr};
#[derive(Debug, Clone)]
pub struct ExprOperator {
    pub op: Operator,
    pub token: Token,
}

impl ExprOperator {
    fn type_check_interface_op(
        self,
        stack: &mut Stack,
        types: &mut TypeMap,
        global_env: &mut GlobalEnv,
        default_sig: Signature,
        interface_fn_name: String,
    ) -> Result<TypedExpr, HayError> {
        match default_sig.evaluate(&self.token, stack, types) {
            Ok(_) => Ok(TypedExpr::Operator {
                op: self.op,
                typ: None,
            }),
            Err(_) => {
                if let Some((StmtKind::InterfaceFunction(base), _)) =
                    global_env.get(&interface_fn_name)
                {
                    let interface = match types.get(base).unwrap() {
                        Type::InterfaceBase(base) => base.clone(),
                        _ => unreachable!(),
                    };

                    let call_expr = ExprIdent {
                        ident: Token {
                            kind: self.token.kind.clone(),
                            lexeme: interface_fn_name,
                            loc: self.token.loc.clone(),
                        },
                    };

                    Ok(TypedExpr::Call {
                        func: interface.resolve(&call_expr, stack, types, global_env)?,
                    })
                } else {
                    panic!(
                        "Interface function `{interface_fn_name}` not found? Error in the prelude?"
                    );
                }
            }
        }
    }

    pub fn type_check(
        self,
        stack: &mut Stack,
        types: &mut TypeMap,
        global_env: &mut GlobalEnv,
    ) -> Result<TypedExpr, HayError> {
        match self.op {
            Operator::Plus => self.type_check_interface_op(
                stack,
                types,
                global_env,
                Signature::new(vec![Type::U64.id(), Type::U64.id()], vec![Type::U64.id()]),
                String::from("Op.add"),
            ),
            Operator::Minus => self.type_check_interface_op(
                stack,
                types,
                global_env,
                Signature::new(vec![Type::U64.id(), Type::U64.id()], vec![Type::U64.id()]),
                String::from("Op.sub"),
            ),
            Operator::Star => self.type_check_interface_op(
                stack,
                types,
                global_env,
                Signature::new(vec![Type::U64.id(), Type::U64.id()], vec![Type::U64.id()]),
                String::from("Op.mul"),
            ),
            Operator::Slash => self.type_check_interface_op(
                stack,
                types,
                global_env,
                Signature::new(vec![Type::U64.id(), Type::U64.id()], vec![Type::U64.id()]),
                String::from("Op.div"),
            ),
            Operator::LessThan
            | Operator::LessEqual
            | Operator::GreaterThan
            | Operator::GreaterEqual => {
                let sigs = vec![
                    // u64 == u64 -> bool
                    Signature::new(vec![Type::U64.id(), Type::U64.id()], vec![Type::Bool.id()]),
                    // u8 == u8   -> bool
                    Signature::new(vec![Type::U8.id(), Type::U8.id()], vec![Type::Bool.id()]),
                    Signature::new(
                        vec![Type::Char.id(), Type::Char.id()],
                        vec![Type::Bool.id()],
                    ),
                ];

                // TODO: Comparison between pointers

                Signature::evaluate_many(&sigs, &self.token, stack, types)?;

                Ok(TypedExpr::Operator {
                    op: self.op,
                    typ: None,
                })
            }
            Operator::Equal => {
                // TODO: equality between Enums

                Signature::evaluate_many(
                    &vec![
                        // u64 == u64 -> bool
                        Signature::new(vec![Type::U64.id(), Type::U64.id()], vec![Type::Bool.id()]),
                        // u8 == u8   -> bool
                        Signature::new(vec![Type::U8.id(), Type::U8.id()], vec![Type::Bool.id()]),
                        // bool == bool -> bool
                        Signature::new(
                            vec![Type::Bool.id(), Type::Bool.id()],
                            vec![Type::Bool.id()],
                        ),
                        // char == char -> char
                        Signature::new(
                            vec![Type::Char.id(), Type::Char.id()],
                            vec![Type::Bool.id()],
                        ),
                        // *T == *T   -> bool
                        Signature::new_generic(
                            vec![TypeId::new("*T"), TypeId::new("*T")],
                            vec![Type::Bool.id()],
                            vec![TypeId::new("T")],
                        ),
                        // &T == &T   -> bool
                        Signature::new_generic(
                            vec![TypeId::new("&T"), TypeId::new("&T")],
                            vec![Type::Bool.id()],
                            vec![TypeId::new("T")],
                        ),
                        Signature::new_generic(
                            vec![TypeId::new("E"), TypeId::new("E")],
                            vec![Type::Bool.id()],
                            vec![TypeId::new("E")],
                        )
                        .with_predicate(
                            &|inputs, types| match (types.get(&inputs[0]), types.get(&inputs[1])) {
                                (
                                    Some(Type::Enum { name: left, .. }),
                                    Some(Type::Enum { name: right, .. }),
                                ) => left.lexeme == right.lexeme,
                                _ => false,
                            },
                            "E is an enum",
                        ),
                    ],
                    &self.token,
                    stack,
                    types,
                )?;

                Ok(TypedExpr::Operator {
                    op: self.op,
                    typ: None,
                })
            }
            Operator::BangEqual => {
                Signature::evaluate_many(
                    &vec![
                        // u64 == u64 -> bool
                        Signature::new(vec![Type::U64.id(), Type::U64.id()], vec![Type::Bool.id()]),
                        // u8 == u8   -> bool
                        Signature::new(vec![Type::U8.id(), Type::U8.id()], vec![Type::Bool.id()]),
                        Signature::new(
                            vec![Type::Char.id(), Type::Char.id()],
                            vec![Type::Bool.id()],
                        ),
                        Signature::new_generic(
                            vec![TypeId::new("*T"), TypeId::new("*T")],
                            vec![Type::Bool.id()],
                            vec![TypeId::new("T")],
                        ),
                        Signature::new_generic(
                            vec![TypeId::new("E"), TypeId::new("E")],
                            vec![Type::Bool.id()],
                            vec![TypeId::new("E")],
                        )
                        .with_predicate(
                            &|inputs, types| match (types.get(&inputs[0]), types.get(&inputs[1])) {
                                (
                                    Some(Type::Enum { name: left, .. }),
                                    Some(Type::Enum { name: right, .. }),
                                ) => left.lexeme == right.lexeme,
                                _ => false,
                            },
                            "E is an enum",
                        ),
                    ],
                    &self.token,
                    stack,
                    types,
                )?;

                Ok(TypedExpr::Operator {
                    op: self.op,
                    typ: None,
                })
            }
            Operator::Modulo => {
                Signature::evaluate_many(
                    &vec![
                        Signature::new(vec![Type::U64.id(), Type::U64.id()], vec![Type::U64.id()]),
                        Signature::new(vec![Type::U8.id(), Type::U8.id()], vec![Type::U8.id()]),
                    ],
                    &self.token,
                    stack,
                    types,
                )?;

                Ok(TypedExpr::Operator {
                    op: self.op,
                    typ: None,
                })
            }
            Operator::Read => {
                let map = Signature::evaluate_many(
                    &vec![
                        Signature::new_generic(
                            vec![TypeId::new("&T")],
                            vec![TypeId::new("T")],
                            vec![TypeId::new("T")],
                        ),
                        Signature::new_generic(
                            vec![TypeId::new("*T")],
                            vec![TypeId::new("T")],
                            vec![TypeId::new("T")],
                        ),
                    ],
                    &self.token,
                    stack,
                    types,
                )?
                .unwrap();

                Ok(TypedExpr::Operator {
                    op: self.op,
                    typ: Some(map.get(&TypeId::new("T")).unwrap().clone()),
                })
            }
            Operator::Write => {
                let map = Signature::new_generic(
                    vec![TypeId::new("T"), TypeId::new("*T")],
                    vec![],
                    vec![TypeId::new("T")],
                )
                .evaluate(&self.token, stack, types)?
                .unwrap();

                Ok(TypedExpr::Operator {
                    op: self.op,
                    typ: Some(map.get(&TypeId::new("T")).unwrap().clone()),
                })
            }
            Operator::Ampersand => todo!("{}", self.token),
            Operator::Unary { .. } => todo!("{}", self.token),
        }
    }
}
