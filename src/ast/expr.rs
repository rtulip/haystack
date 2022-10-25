use crate::ast::arg::Arg;
use crate::error::HayError;
use crate::lex::token::{Literal, Operator, Token, TokenKind};
use crate::types::{RecordKind, Signature, Type, TypeId, Untyped};
use std::collections::{BTreeMap, HashMap};

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
        then: Vec<Expr<TypeState>>,
        otherwise: Vec<Expr<TypeState>>,
        finally: Option<Vec<Expr<TypeState>>>,
    },
    ElseIf {
        else_tok: Token,
        condition: Vec<Expr<TypeState>>,
        block: Vec<Expr<TypeState>>,
    },
    As {
        token: Token,
        args: Vec<Arg<TypeState>>,
        block: Option<Vec<Expr<TypeState>>>,
    },
    Var {
        token: Token,
        typ: Token,
        ident: Token,
    },
    While {
        token: Token,
        cond: Vec<Expr<TypeState>>,
        body: Vec<Expr<TypeState>>,
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

impl UntypedExpr {
    pub fn type_check(
        &self,
        stack: &mut Vec<TypeId>,
        frame: &mut Vec<(String, TypeId)>,
        global_env: &HashMap<String, Signature>,
        types: &mut BTreeMap<TypeId, Type>,
        generic_map: &Option<HashMap<TypeId, TypeId>>,
    ) -> Result<(), HayError> {
        let r = match self {
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
                } else if let Some(Type::Enum { variants, .. }) =
                    types.get(&TypeId::new(&ident.lexeme))
                {
                    if inner.len() != 1 {
                        return Err(HayError::new(
                            "Cannot have multiple inner accessor for an enum type.",
                            token.loc.clone(),
                        )
                        .with_hint(format!("Found accessors: {:?}", inner))
                        .with_hint(format!(
                            "Enum {} has variants: {:?}",
                            ident.lexeme, variants
                        )));
                    }

                    if !variants.iter().any(|v| v.lexeme == inner[0].lexeme) {
                        return Err(HayError::new(
                            format!("Unknown enum variant {}", inner[0].lexeme),
                            token.loc.clone(),
                        )
                        .with_hint(format!(
                            "Enum {} has variants: {:?}",
                            ident.lexeme, variants
                        )));
                    }

                    stack.push(TypeId::new(&ident.lexeme));

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
            } => {
                let mut sig = global_env.get(&base.lexeme).unwrap().clone();

                let annotations = if let Some(map) = generic_map {
                    let ann = annotations
                        .iter()
                        .map(|arg| {
                            let tid = TypeId::new(&arg.token.lexeme);
                            tid.assign(token, map, types)
                        })
                        .collect::<Vec<Result<TypeId, HayError>>>();

                    let gen_fn_tid = TypeId::new(&base.lexeme);
                    gen_fn_tid.assign(token, map, types)?;

                    ann
                } else {
                    annotations
                        .iter()
                        .map(|arg| Ok(TypeId::new(&arg.token.lexeme)))
                        .collect::<Vec<Result<TypeId, HayError>>>()
                };

                for ann in &annotations {
                    if let Err(e) = ann {
                        return Err(e.clone());
                    }
                }

                let annotations = annotations
                    .into_iter()
                    .map(|a| if let Ok(tid) = a { tid } else { unreachable!() })
                    .collect::<Vec<TypeId>>();

                sig.assign(token, &annotations, types)?;

                if let Some(_map) = sig.evaluate(token, stack, types)? {
                    todo!("Make a concrete version of the call")
                }

                Ok(())
            }
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
                        e.type_check(stack, frame, global_env, types, generic_map)?;
                    }

                    for _ in 0..args.len() {
                        frame.pop();
                    }
                }

                Ok(())
            }
            Expr::Cast { token, typ } => {
                let typ_id = TypeId::new(&typ.lexeme);
                let typ_id = if let Some(map) = generic_map {
                    if let Ok(tid) = typ_id.assign(token, map, types) {
                        // try to assign for annotated casts
                        tid
                    } else {
                        // If annotation fails, need to use resolution.
                        typ_id
                    }
                } else {
                    typ_id
                };
                let cast_type = types.get(&typ_id).unwrap().clone();

                match &cast_type {
                    Type::Record { members, kind, .. } => match kind {
                        RecordKind::Struct => {
                            Signature::new(
                                members.iter().map(|m| m.typ.0.clone()).collect(),
                                vec![typ_id],
                            )
                            .evaluate(token, stack, types)?;
                            Ok(())
                        }
                        RecordKind::Union => {
                            let mut sigs = vec![];

                            members.iter().for_each(|m| {
                                sigs.push(Signature::new(
                                    vec![m.typ.0.clone()],
                                    vec![typ_id.clone()],
                                ));
                            });

                            Signature::evaluate_many(&sigs, token, stack, types)?;
                            Ok(())
                        }
                    },
                    Type::GenericRecordInstance { kind, .. } => {
                        if let Some(generic_map) = generic_map {
                            let new_typ = typ_id.assign(token, generic_map, types)?;

                            if let Some(Type::Record { members, kind, .. }) = types.get(&new_typ) {
                                match kind {
                                    RecordKind::Struct => {
                                        Signature::new(
                                            members.iter().map(|m| m.typ.0.clone()).collect(),
                                            vec![new_typ],
                                        )
                                        .evaluate(token, stack, types)?;
                                        Ok(())
                                    }
                                    RecordKind::Union => {
                                        let mut sigs = vec![];

                                        members.iter().for_each(|m| {
                                            sigs.push(Signature::new(
                                                vec![m.typ.0.clone()],
                                                vec![new_typ.clone()],
                                            ));
                                        });

                                        Signature::evaluate_many(&sigs, token, stack, types)?;
                                        Ok(())
                                    }
                                }
                            } else {
                                unreachable!()
                            }
                        } else {
                            return Err(HayError::new(format!("Shouldn't be able to cast to {kind} {typ_id} without a known mapping for generics"), token.loc.clone()));
                        }
                    }
                    Type::U64 => {
                        Signature::evaluate_many(
                            &vec![
                                Signature::new(vec![Type::U64.id()], vec![Type::U64.id()]),
                                Signature::new(vec![Type::U8.id()], vec![Type::U64.id()]),
                                Signature::new(vec![Type::Bool.id()], vec![Type::U64.id()]),
                                Signature::new(vec![Type::Char.id()], vec![Type::U64.id()]),
                                Signature::new_maybe_generic(
                                    vec![TypeId::new("*T")],
                                    vec![Type::U64.id()],
                                    Some(vec![TypeId::new("T")]),
                                ),
                            ],
                            token,
                            stack,
                            types,
                        )?;
                        Ok(())
                    }
                    Type::U8 => {
                        Signature::evaluate_many(
                            &vec![
                                Signature::new(vec![Type::U64.id()], vec![Type::U8.id()]),
                                Signature::new(vec![Type::U8.id()], vec![Type::U8.id()]),
                                Signature::new(vec![Type::Bool.id()], vec![Type::U8.id()]),
                                Signature::new(vec![Type::Char.id()], vec![Type::U8.id()]),
                            ],
                            token,
                            stack,
                            types,
                        )?;
                        Ok(())
                    }
                    Type::Pointer { .. } => {
                        Signature::new(vec![Type::U64.id()], vec![typ_id])
                            .evaluate(token, stack, types)?;
                        Ok(())
                    }
                    Type::GenericRecordBase {
                        generics, members, ..
                    } => {
                        Signature::new_maybe_generic(
                            members.iter().map(|m| m.typ.0.clone()).collect(),
                            vec![typ_id.clone()],
                            Some(generics.clone()),
                        )
                        .evaluate(token, stack, types)?;
                        Ok(())
                    }
                    t => unimplemented!("casting to {:?} is unimplemented", t),
                }
            }
            Expr::ElseIf {
                else_tok,
                condition,
                block,
            } => {
                for expr in condition {
                    expr.type_check(stack, frame, global_env, types, generic_map)?;
                }

                Signature::new(vec![Type::Bool.id()], vec![]).evaluate(else_tok, stack, types)?;

                for expr in block {
                    expr.type_check(stack, frame, global_env, types, generic_map)?;
                }

                Ok(())
            }
            Expr::Ident { ident } => {
                if let Some(sig) = global_env.get(&ident.lexeme) {
                    if let Some(map) = sig.evaluate(ident, stack, types)? {
                        let gen_fn_tid = TypeId::new(&ident.lexeme);
                        gen_fn_tid.assign(ident, &map, types)?;
                    }

                    return Ok(());
                }

                if let Some((_, tid)) = frame.iter().find(|(id, _)| &ident.lexeme == id) {
                    stack.push(tid.clone());
                    return Ok(());
                }

                return Err(HayError::new_type_err(
                    format!("Unrecognized word `{}`", ident.lexeme),
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
                sig.evaluate(token, stack, types)?;

                let initial_stack = stack.clone();
                let initial_frame = frame.clone();

                let mut end_stacks = vec![];

                for e in then {
                    e.type_check(stack, frame, global_env, types, generic_map)?;
                }

                end_stacks.push((token.clone(), stack.clone()));

                for case in otherwise {
                    *stack = initial_stack.clone();
                    *frame = initial_frame.clone();

                    case.type_check(stack, frame, global_env, types, generic_map)?;

                    end_stacks.push((case.token().clone(), stack.clone()));
                }

                if let Some(finally) = finally {
                    *stack = initial_stack;
                    *frame = initial_frame;
                    for e in finally {
                        e.type_check(stack, frame, global_env, types, generic_map)?;
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
                    return Err(HayError::new(
                        format!(
                            "Logic Error -- Literal without TokenKind::Literal. Found {} instead",
                            value.kind
                        ),
                        value.loc.clone(),
                    ));
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

                            Signature::evaluate_many(&sigs, op_tok, stack, types)?;

                            Ok(())
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
                                // *T == *T    -> bool
                                Signature::new_maybe_generic(
                                    vec![TypeId::new("*T"), TypeId::new("*T")],
                                    vec![Type::U64.id()],
                                    Some(vec![TypeId::new("T")]),
                                ),
                            ];

                            // TODO: Comparison between pointers

                            Signature::evaluate_many(&sigs, op_tok, stack, types)?;

                            Ok(())
                        }
                        Operator::Star => {
                            Signature::evaluate_many(
                                &vec![
                                    Signature::new(
                                        vec![Type::U64.id(), Type::U64.id()],
                                        vec![Type::U64.id()],
                                    ),
                                    Signature::new(
                                        vec![Type::U8.id(), Type::U8.id()],
                                        vec![Type::U8.id()],
                                    ),
                                ],
                                op_tok,
                                stack,
                                types,
                            )?;

                            Ok(())
                        }
                        Operator::Slash => {
                            Signature::evaluate_many(
                                &vec![
                                    Signature::new(
                                        vec![Type::U64.id(), Type::U64.id()],
                                        vec![Type::U64.id()],
                                    ),
                                    Signature::new(
                                        vec![Type::U8.id(), Type::U8.id()],
                                        vec![Type::U8.id()],
                                    ),
                                ],
                                op_tok,
                                stack,
                                types,
                            )?;

                            Ok(())
                        }
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

                            Signature::evaluate_many(&sigs, op_tok, stack, types)?;

                            Ok(())
                        }
                        Operator::Equal => {
                            // TODO: equality between Enums

                            Signature::evaluate_many(
                                &vec![
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
                                    // *T == *T   -> bool
                                    Signature::new_maybe_generic(
                                        vec![TypeId::new("*T"), TypeId::new("*T")],
                                        vec![Type::Bool.id()],
                                        Some(vec![TypeId::new("T")]),
                                    ),
                                    Signature::new_maybe_generic(
                                        vec![TypeId::new("E"), TypeId::new("E")],
                                        vec![Type::Bool.id()],
                                        Some(vec![TypeId::new("E")]),
                                    )
                                    .with_predicate(
                                        &|inputs, types| match (
                                            types.get(&inputs[0]),
                                            types.get(&inputs[1]),
                                        ) {
                                            (
                                                Some(Type::Enum { name: left, .. }),
                                                Some(Type::Enum { name: right, .. }),
                                            ) => left.lexeme == right.lexeme,
                                            _ => false,
                                        },
                                        "E is an enum",
                                    ),
                                ],
                                op_tok,
                                stack,
                                types,
                            )?;

                            Ok(())
                        }
                        Operator::BangEqual => {
                            Signature::evaluate_many(
                                &vec![
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
                                    Signature::new_maybe_generic(
                                        vec![TypeId::new("*T"), TypeId::new("*T")],
                                        vec![Type::Bool.id()],
                                        Some(vec![TypeId::new("T")]),
                                    ),
                                    Signature::new_maybe_generic(
                                        vec![TypeId::new("E"), TypeId::new("E")],
                                        vec![Type::Bool.id()],
                                        Some(vec![TypeId::new("E")]),
                                    )
                                    .with_predicate(
                                        &|inputs, types| match (
                                            types.get(&inputs[0]),
                                            types.get(&inputs[1]),
                                        ) {
                                            (
                                                Some(Type::Enum { name: left, .. }),
                                                Some(Type::Enum { name: right, .. }),
                                            ) => left.lexeme == right.lexeme,
                                            _ => false,
                                        },
                                        "E is an enum",
                                    ),
                                ],
                                op_tok,
                                stack,
                                types,
                            )?;

                            Ok(())
                        }
                        Operator::Modulo => {
                            Signature::evaluate_many(
                                &vec![
                                    Signature::new(
                                        vec![Type::U64.id(), Type::U64.id()],
                                        vec![Type::U64.id()],
                                    ),
                                    Signature::new(
                                        vec![Type::U8.id(), Type::U8.id()],
                                        vec![Type::U8.id()],
                                    ),
                                ],
                                op_tok,
                                stack,
                                types,
                            )?;

                            Ok(())
                        }
                        Operator::Read => {
                            Signature::new_maybe_generic(
                                vec![TypeId::new("*T")],
                                vec![TypeId::new("T")],
                                Some(vec![TypeId::new("T")]),
                            )
                            .evaluate(op_tok, stack, types)?;

                            Ok(())
                        }
                        Operator::Write => {
                            Signature::new_maybe_generic(
                                vec![TypeId::new("T"), TypeId::new("*T")],
                                vec![],
                                Some(vec![TypeId::new("T")]),
                            )
                            .evaluate(op_tok, stack, types)?;

                            Ok(())
                        }
                    }
                }
                _ => {
                    return Err(HayError::new(
                        "Logic error. Operator token with kind != TokenKind::Operator",
                        op_tok.loc.clone(),
                    ))
                }
            },
            Expr::SizeOf { token, .. } => {
                Signature::new(vec![], vec![Type::U64.id()]).evaluate(token, stack, types)?;
                Ok(())
            }
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

                Signature::new(vec![Type::U64.id()], vec![]).evaluate(token, stack, types)?;

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
            Expr::Var { typ, ident, .. } => {
                let typ_id = TypeId::from_token(typ, types, &vec![])?;
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
            Expr::While { token, cond, body } => {
                let stack_before = stack.clone();

                // Evaluate up to the body
                for expr in cond {
                    expr.type_check(stack, frame, global_env, types, generic_map)?;
                }

                Signature::new(vec![Type::Bool.id()], vec![]).evaluate(token, stack, types)?;

                for expr in body {
                    expr.type_check(stack, frame, global_env, types, generic_map)?;
                }

                if stack.iter().zip(&stack_before).any(|(t1, t2)| t1 != t2) {
                    return Err(HayError::new(
                        "While loop must not change stack between iterations.",
                        token.loc.clone(),
                    )
                    .with_hint(format!("Stack before loop: {:?}", stack_before))
                    .with_hint(format!("Stack after loop:  {:?}", stack)));
                }

                Ok(())
            }
        };

        r
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
