use crate::ast::arg::Arg;
use crate::error::HayError;
use crate::lex::token::{Literal, Loc, Operator, Token, TokenKind};
use crate::types::{RecordKind, Signature, Type, TypeId, Untyped};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Expr<TypeState> {
    Literal {
        value: Token,
    },
    Operator {
        op: Token,
    },
    Syscall {
        token: Token,
        n: usize,
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
        then: Vec<Box<Expr<TypeState>>>,
        otherwise: Vec<Box<Expr<TypeState>>>,
        finally: Option<Vec<Box<Expr<TypeState>>>>,
    },
    ElseIf {
        else_tok: Token,
        condition: Vec<Box<Expr<TypeState>>>,
        block: Vec<Box<Expr<TypeState>>>,
    },
    As {
        token: Token,
        args: Vec<Arg<TypeState>>,
        block: Option<Vec<Box<Expr<TypeState>>>>,
    },
    Var {
        token: Token,
        typ: Token,
        ident: Token,
    },
    While {
        token: Token,
        cond: Vec<Box<Expr<TypeState>>>,
        body: Vec<Box<Expr<TypeState>>>,
    },
    AnnotatedCall {
        token: Token,
        base: Token,
        annotations: Vec<Arg<TypeState>>,
    },
    SizeOf {
        token: Token,
        typ: Token,
    },
}

impl<TS> Expr<TS> {
    pub fn token(&self) -> &Token {
        match self {
            Expr::Literal { value: token }
            | Expr::Operator { op: token }
            | Expr::Syscall { token, .. }
            | Expr::Cast { token, .. }
            | Expr::Ident { ident: token, .. }
            | Expr::Accessor { token, .. }
            | Expr::If { token, .. }
            | Expr::ElseIf {
                else_tok: token, ..
            }
            | Expr::As { token, .. }
            | Expr::Var { token, .. }
            | Expr::While { token, .. }
            | Expr::AnnotatedCall { token, .. }
            | Expr::SizeOf { token, .. } => token,
        }
    }
}

pub type UntypedExpr = Expr<Untyped>;
pub struct TypedExpr {
    expr: UntypedExpr,
    stack: Signature,
    frame: Signature,
}

impl TypedExpr {
    pub fn new(expr: &UntypedExpr, stack: Signature, frame: Signature) -> Self {
        Self {
            expr: expr.clone(),
            stack,
            frame,
        }
    }
}

impl UntypedExpr {
    pub fn type_check(
        &self,
        stack: &mut Vec<TypeId>,
        frame: &mut Vec<(String, TypeId)>,
        global_env: &HashMap<String, Signature>,
        types: &mut HashMap<TypeId, Type>,
    ) -> Result<(), HayError> {
        match self {
            Expr::Accessor {
                token,
                ident,
                inner,
            } => {
                if let Some((_, tid)) = frame.iter().find(|(k, _)| k == &ident.lexeme) {
                    let mut typ = tid;
                    for inner_member in inner {
                        if let Type::Record { name, members, .. } = types.get(typ).unwrap() {
                            if let Some(m) = members
                                .iter()
                                .find(|m| m.ident.lexeme == inner_member.lexeme)
                            {
                                typ = &m.typ.0;
                            } else {
                                return Err(HayError::new(
                                    format!(
                                        "Type `{}` doesn't have a member {}",
                                        name.lexeme, inner_member.lexeme,
                                    ),
                                    token.loc.clone(),
                                ));
                            }
                        } else {
                            return Err(HayError::new(
                                format!("Cannot access into non-record type `{tid}`"),
                                token.loc.clone(),
                            ));
                        }
                    }

                    stack.push(typ.clone());
                    Ok(())
                } else {
                    return Err(HayError::new(
                        format!("Unknown identifier `{}`", ident.lexeme),
                        token.loc.clone(),
                    ));
                }
            }
            Expr::AnnotatedCall {
                token,
                base,
                annotations,
            } => todo!(),
            Expr::As { token, args, block } => {
                if stack.len() < args.len() {
                    let e = HayError::new_type_err(
                        "Insufficient elements on the stack to bind",
                        token.loc.clone(),
                    )
                    .with_hint(format!(
                        "Expected {} elements to bind to idents: {:?}",
                        args.len(),
                        args.iter()
                            .map(|arg| &arg.token.lexeme)
                            .collect::<Vec<&String>>()
                    ))
                    .with_hint(format!("Found: {:?}", stack));

                    return Err(e);
                }

                args.iter().rev().for_each(|arg| {
                    let t = stack.pop().unwrap();
                    frame.push((arg.token.lexeme.clone(), t));
                });

                if let Some(blk) = block {
                    for e in blk {
                        e.type_check(stack, frame, global_env, types)?;
                    }

                    for _ in 0..args.len() {
                        frame.pop();
                    }
                }

                Ok(())
            }
            Expr::Cast { token, typ } => {
                let typ_id = TypeId::new(&typ.lexeme);
                let cast_type = types.get(&typ_id).unwrap();

                match &cast_type {
                    Type::Record { members, kind, .. } => match kind {
                        RecordKind::Struct => Signature::new(
                            members.iter().map(|m| m.typ.0.clone()).collect(),
                            vec![typ_id],
                        )
                        .evaluate(token, stack),
                        RecordKind::Union => todo!("Casting to unions"),
                    },
                    Type::GenericRecordInstance { .. } => {
                        unimplemented!("Casting to generic record instance is unimplemented")
                    }
                    Type::U64 => todo!("Cast to u64"),
                    Type::Pointer { inner } => {
                        // TODO: check if pointer is generic
                        Signature::new(vec![Type::U64.id()], vec![typ_id]).evaluate(token, stack)
                    }
                    t => unimplemented!("casting to {:?} is unimplemented", t),
                }
            }
            Expr::ElseIf {
                else_tok,
                condition,
                block,
            } => todo!(),
            Expr::Ident { ident } => {
                if let Some(sig) = global_env.get(&ident.lexeme) {
                    sig.evaluate(ident, stack)?;
                    return Ok(());
                }

                if let Some((_, tid)) = frame.iter().find(|(id, _)| &ident.lexeme == id) {
                    stack.push(tid.clone());
                    return Ok(());
                }

                return Err(HayError::new_type_err(
                    format!("Unrecognized word `{:?}`", ident.lexeme),
                    ident.loc.clone(),
                ));
            }
            Expr::If {
                token,
                then,
                otherwise,
                finally,
            } => {
                let sig = Signature::new(vec![Type::Bool.id()], vec![]);
                sig.evaluate(token, stack)?;

                let initial_stack = stack.clone();
                let initial_frame = frame.clone();

                let mut end_stacks = vec![];

                for e in then {
                    e.type_check(stack, frame, global_env, types)?;
                }

                end_stacks.push((token.clone(), stack.clone()));

                for case in otherwise {
                    *stack = initial_stack.clone();
                    *frame = initial_frame.clone();

                    case.type_check(stack, frame, global_env, types)?;

                    end_stacks.push((case.token().clone(), stack.clone()));
                }

                if let Some(finally) = finally {
                    *stack = initial_stack.clone();
                    *frame = initial_frame.clone();
                    for e in finally {
                        e.type_check(stack, frame, global_env, types)?;
                    }

                    end_stacks.push((finally[0].token().clone(), stack.clone()));
                }

                if !(0..end_stacks.len() - 1)
                    .into_iter()
                    .all(|i| end_stacks[i].1 == end_stacks[i + 1].1)
                {
                    let mut err = HayError::new_type_err(
                        "If block creates stacks of diferent shapes",
                        token.loc.clone(),
                    )
                    .with_hint("Each branch of if block must evaluate to the same stack layout.");

                    for (i, (tok, stk)) in end_stacks.iter().enumerate() {
                        err = err.with_hint(format!("{} Branch {}: {:?}", tok.loc, i + 1, stk));
                    }

                    return Err(err);
                }

                Ok(())
            }
            Expr::Literal { value } => {
                if let TokenKind::Literal(lit) = &value.kind {
                    match lit {
                        Literal::Bool(_) => stack.push(Type::Bool.id()),
                        Literal::Char(_) => stack.push(Type::Char.id()),
                        Literal::U64(_) => stack.push(Type::U64.id()),
                        Literal::U8(_) => stack.push(Type::U8.id()),
                        Literal::String(_) => stack.push(TypeId::new("Str")),
                    }

                    Ok(())
                } else {
                    return Err(HayError::new("Hey what the fuck???", value.loc.clone()));
                }
            }
            Expr::Operator { op: op_tok } => match &op_tok.kind {
                TokenKind::Operator(op) => {
                    match op {
                        Operator::Plus => {
                            let sigs = vec![
                                // u64 + u64 -> u64
                                Signature::new(
                                    vec![Type::U64.id(), Type::U64.id()],
                                    vec![Type::U64.id()],
                                ),
                                // u8 + u8   -> u8
                                Signature::new(
                                    vec![Type::U8.id(), Type::U8.id()],
                                    vec![Type::U8.id()],
                                ),
                                // u64 + u8  -> u64
                                Signature::new(
                                    vec![Type::U64.id(), Type::U8.id()],
                                    vec![Type::U64.id()],
                                ),
                                // u8 + u64  -> u64
                                Signature::new(
                                    vec![Type::U8.id(), Type::U64.id()],
                                    vec![Type::U64.id()],
                                ),
                            ];

                            Signature::evaluate_many(&sigs, op_tok, stack)
                        }
                        Operator::Minus => {
                            let sigs = vec![
                                // u64 == u64 -> bool
                                Signature::new(
                                    vec![Type::U64.id(), Type::U64.id()],
                                    vec![Type::U64.id()],
                                ),
                                // u8 == u8   -> bool
                                Signature::new(
                                    vec![Type::U8.id(), Type::U8.id()],
                                    vec![Type::U8.id()],
                                ),
                            ];

                            // TODO: Comparison between pointers

                            Signature::evaluate_many(&sigs, op_tok, stack)
                        }
                        Operator::Star => todo!(),
                        Operator::Slash => todo!(),
                        Operator::LessThan
                        | Operator::LessEqual
                        | Operator::GreaterThan
                        | Operator::GreaterEqual => {
                            let sigs = vec![
                                // u64 == u64 -> bool
                                Signature::new(
                                    vec![Type::U64.id(), Type::U64.id()],
                                    vec![Type::Bool.id()],
                                ),
                                // u8 == u8   -> bool
                                Signature::new(
                                    vec![Type::U8.id(), Type::U8.id()],
                                    vec![Type::Bool.id()],
                                ),
                            ];

                            // TODO: Comparison between pointers

                            Signature::evaluate_many(&sigs, op_tok, stack)
                        }
                        Operator::Equal => {
                            let sigs = vec![
                                // u64 == u64 -> bool
                                Signature::new(
                                    vec![Type::U64.id(), Type::U64.id()],
                                    vec![Type::Bool.id()],
                                ),
                                // u8 == u8   -> bool
                                Signature::new(
                                    vec![Type::U8.id(), Type::U8.id()],
                                    vec![Type::Bool.id()],
                                ),
                            ];

                            // TODO: equality between Enums
                            // TODO: Equality between pointers

                            Signature::evaluate_many(&sigs, op_tok, stack)
                        }
                        Operator::BangEqual => todo!(),
                        Operator::Modulo => todo!(),
                        Operator::Read => todo!(),
                        Operator::Write => todo!(),
                    }
                }
                _ => {
                    return Err(HayError::new(
                        "Logic error. Operator token with kind != TokenKind::Operator",
                        op_tok.loc.clone(),
                    ))
                }
            },
            Expr::SizeOf { token, typ } => todo!(),
            Expr::Syscall { token, n } => {
                if stack.len() < *n as usize {
                    return Err(HayError::new_type_err(
                        format!(
                            "{} requires at least {} elements on the stack. Found {}",
                            token.lexeme,
                            n + 1,
                            stack.len()
                        ),
                        token.loc.clone(),
                    ));
                }

                Signature::new(vec![Type::U64.id()], vec![]).evaluate(token, stack)?;

                for _ in 0..*n {
                    let t = stack.pop().unwrap();
                    let size = t.size(types)?;
                    if size != 1 {
                        return Err(HayError::new(
                            format!("`{}` can only accept types of size 1.", token.lexeme),
                            token.loc.clone(),
                        )
                        .with_hint(format!("Found type `{t}` which has size {size}")));
                    }
                }

                stack.push(Type::U64.id());

                Ok(())
            }
            Expr::Var { token, typ, ident } => {
                let typ_id = TypeId::new(&typ.lexeme);
                if types.get(&typ_id).is_none() {
                    return Err(HayError::new(
                        format!("Unrecognized type `{typ_id}`"),
                        typ.loc.clone(),
                    ));
                }

                let ptr = Type::Pointer { inner: typ_id };
                let id = ptr.id();

                if !types.contains_key(&id) {
                    types.insert(id.clone(), ptr);
                }

                frame.push((ident.lexeme.clone(), id));

                Ok(())
            }
            Expr::While { token, cond, body } => todo!(),
        }
    }
}

impl<TypeState> std::fmt::Display for Expr<TypeState> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal { value: token }
            | Expr::Operator { op: token }
            | Expr::Syscall { token, .. }
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
