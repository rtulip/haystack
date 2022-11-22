use crate::error::HayError;
use crate::lex::token::{Keyword, Literal, Operator, Token};
use crate::types::{FramedType, RecordKind, Signature, Type, TypeId, TypeMap, UncheckedFunction};
use std::collections::HashMap;
use super::arg::{IdentArg, UntypedArg};
use super::stmt::StmtKind;

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
/// * [`Expr::ElseIf`] represents the code executed in a non-final else-branch
/// * [`Expr::As`] represents the action of binding values to be framed
/// * [`Expr::Var`] represents creating vars at a function level
/// * [`Expr::While`] represents a whlie loop including and the associated conditional.
/// * [`Expr::AnnotatedCall`] Similar to [`Expr::Accessor`], but for specifying types to function calls.
/// * [`Expr::SizeOf`] is for taking the size of a type
/// * [`Expr::Return`] returns from a function.
#[derive(Debug, Clone)]
pub enum Expr {
    Literal(ExprLiteral),
    Operator(ExprOperator),
    Unary(ExprUnary),
    Syscall(ExprSyscall),
    Cast(ExprCast),
    Ident(ExprIdent),
    Accessor(ExprAccessor),
    If(ExprIf),
    As(ExprAs),
    Var(ExprVar),
    While(ExprWhile),
    AnnotatedCall(ExprAnnotatedCall),
    SizeOf(ExprSizeOf),
    Return(ExprReturn),
}

#[derive(Debug, Clone)]
pub struct ExprLiteral {
    pub literal: Literal,
    pub token: Token,
}

#[derive(Debug, Clone)]
pub struct ExprOperator {
    pub op: Operator,
    pub token: Token,
}

#[derive(Debug, Clone)]
pub struct ExprUnary {
    pub op: ExprOperator,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprSyscall {
    pub n: usize,
    pub token: Token,
}

#[derive(Debug, Clone)]
pub struct ExprCast {
    pub token: Token,
    pub typ: Token,
}

#[derive(Debug, Clone)]
pub struct ExprIdent {
    pub ident: Token,
}

#[derive(Debug, Clone)]
pub struct ExprAccessor {
    pub token: Token,
    pub ident: Token,
    pub inner: Vec<Token>,
}

#[derive(Debug, Clone)]
pub struct ExprSizeOf {
    /// The token of the `sizeOf` keyword
    pub token: Token,
    /// The token of the type
    pub typ: Token,
}

#[derive(Debug, Clone)]
pub struct ExprIf {
    /// Token of the `If` keyword
    pub token: Token,
    /// A list of expressions to execute if true.
    pub then: Vec<Expr>,
    /// A list of expressions for each else-if case
    /// Note: these are guaranteed to be [`Expr::ElseIf`]
    pub otherwise: Vec<ExprElseIf>,
    /// An optional final `else` case.
    pub finally: Option<Vec<Expr>>,
}

#[derive(Debug, Clone)]
pub struct ExprElseIf {
    /// Token of the `else` keyword
    pub token: Token,
    /// The expressions to evaluate before the next `if`.
    pub condition: Vec<Expr>,
    /// The body of the `else` expression
    pub block: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprAs {
    /// Token of the `as` keyword
    pub token: Token,
    /// The non-empty list of identifiers.
    pub idents: Vec<IdentArg>,
    /// The optional temporary scope.
    pub block: Option<Vec<Expr>>,    
}

#[derive(Debug, Clone)]
pub struct ExprVar  {
    /// The token of the `var` keyword
    pub token: Token,
    /// The token of the type of the var
    pub typ: Token,
    /// The token of the name of the var.
    pub ident: Token,
}

#[derive(Debug, Clone)]
pub struct ExprWhile {
    /// The token of the `while` keyword
    pub token: Token,
    /// The condition expressions before the body
    pub cond: Vec<Expr>,
    /// The body of the `while` loop.
    pub body: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprAnnotatedCall {
    /// The token for the entire annotated call.
    pub token: Token,
    /// The base identifier token
    pub base: Token,
    /// The list of annotations
    pub annotations: Vec<UntypedArg>,
}

#[derive(Debug, Clone)]
pub struct ExprReturn {
    pub token: Token,
}

impl Expr {
    
    /// Helper function to quickly get the most pertinent token from an [`Expr`]
    pub fn token(&self) -> &Token {
        match self {
            Expr::Literal(ExprLiteral { token, .. })
            | Expr::Operator(ExprOperator { token, .. })
            | Expr::Syscall(ExprSyscall { token, .. })
            | Expr::Cast(ExprCast { token, .. })
            | Expr::Ident(ExprIdent { ident: token, .. })
            | Expr::Accessor(ExprAccessor { token, .. })
            | Expr::If(ExprIf { token, .. })
            | Expr::As(ExprAs { token, .. })
            | Expr::Var(ExprVar { token, .. })
            | Expr::While(ExprWhile { token, .. })
            | Expr::AnnotatedCall(ExprAnnotatedCall { token, .. })
            | Expr::SizeOf(ExprSizeOf { token, .. })
            | Expr::Return(ExprReturn { token }) 
            | Expr::Unary(ExprUnary { op: ExprOperator { token, .. }, ..}) => token,
        }
    }


    fn type_check_accessor_expression(
        token: Token,
        ident: Token,
        inner: Vec<Token>,
        stack: &mut Vec<TypeId>,
        frame: &mut Vec<(String, FramedType)>,
        func: &UncheckedFunction,
        types: &mut TypeMap,
    ) -> Result<TypedExpr, HayError> {
        // Three cases for an accessor
        // 1. Ident is a framed record or pointer
        // 2. Ident is an enum found in types.
        // 3. Ident is unrecognized.

        if let Some((i, (_, ft))) = frame
            .iter()
            .enumerate()
            .find(|(_, (k, _))| k == &ident.lexeme)
        {
            match types.get(&ft.typ).unwrap() {
                Type::Record { .. } => {
                    let final_tid = ft
                        .typ
                        .type_check_inner_accessors(token, &inner, func, types)?;

                    stack.push(final_tid);
                    Ok(TypedExpr::Framed {
                        frame: frame.clone(),
                        idx: i,
                        inner: Some(inner.iter().map(|t| t.lexeme.clone()).collect()),
                    })
                }
                Type::Pointer {
                    inner: pointer_inner_tid,
                    mutable: pointer_inner_mut,
                } => {
                    let final_tid = pointer_inner_tid
                        .type_check_inner_accessors(token, &inner, func, types)?;

                    let ptr_type = Type::Pointer {
                        inner: final_tid,
                        mutable: *pointer_inner_mut,
                    };
                    let ptr_tid = ptr_type.id();
                    types.insert(ptr_type.id(), ptr_type);
                    stack.push(ptr_tid);
                    Ok(TypedExpr::FramedPointerOffset {
                        frame: frame.clone(),
                        idx: i,
                        inner: inner.iter().map(|t| t.lexeme.clone()).collect(),
                    })
                }
                _ => Err(HayError::new_type_err(
                    format!("Cannot access into non-record type `{ft}`"),
                    token.loc,
                )),
            }
        } else if let Some(Type::Enum { variants, .. }) =
            types.get(&TypeId::new(&ident.lexeme))
        {
            if inner.len() != 1 {
                return Err(HayError::new(
                    "Cannot have multiple inner accessor for an enum type.",
                    token.loc,
                )
                .with_hint(format!(
                    "Found accessors: {:?}",
                    inner.iter().map(|t| &t.lexeme).collect::<Vec<&String>>()
                )));
            }

            if !variants.iter().any(|v| v.lexeme == inner[0].lexeme) {
                return Err(HayError::new(
                    format!("Unknown enum variant `{}`", inner[0].lexeme),
                    token.loc,
                )
                .with_hint(format!(
                    "Enum {} has variants: {:?}",
                    ident.lexeme,
                    variants.iter().map(|t| &t.lexeme).collect::<Vec<&String>>()
                )));
            }

            stack.push(TypeId::new(&ident.lexeme));

            Ok(TypedExpr::Enum {
                typ: TypeId::new(&ident.lexeme),
                variant: variants
                    .iter()
                    .find(|t| t.lexeme == inner[0].lexeme)
                    .unwrap()
                    .lexeme
                    .clone(),
            })
        } else {
            Err(HayError::new(
                format!("Unknown identifier `{}`", ident.lexeme),
                token.loc,
            ))
        }
    }

    /// Type checks an expression
    pub fn type_check(
        self,
        stack: &mut Vec<TypeId>,
        frame: &mut Vec<(String, FramedType)>,
        func: &UncheckedFunction,
        global_env: &HashMap<String, (StmtKind, Signature)>,
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
            Expr::Accessor(ExprAccessor {
                token,
                ident,
                inner,
            }) => {
                Expr::type_check_accessor_expression(
                    token, ident, inner,
                    stack,
                    frame,
                    func,
                    types
                )
            }
            Expr::AnnotatedCall(ExprAnnotatedCall {
                token,
                base,
                annotations,
            }) => {
                let (_, mut sig) = global_env
                    .get(&base.lexeme)
                    .unwrap_or_else(|| panic!("Should have found function: {base}"))
                    .clone();

                let (annotations, tid) = if annotations
                    .iter()
                    .any(|arg| TypeId::new(&arg.token.lexeme).is_generic(types))
                {
                    if let Some(map) = generic_map {
                        let ann = annotations
                            .iter()
                            .map(|arg| {
                                let tid = TypeId::new(&arg.token.lexeme);
                                tid.assign(&token, map, types)
                            })
                            .collect::<Vec<Result<TypeId, HayError>>>();

                        let gen_fn_tid = TypeId::new(&base.lexeme);
                        let func = gen_fn_tid.assign(&token, map, types)?;

                        (ann, func)
                    } else {
                        return Err(HayError::new_type_err(
                            "Unresolved generic types in annotations",
                            token.loc.clone(),
                        )
                        .with_hint(format!(
                            "Found annotations: {:?}",
                            annotations
                                .iter()
                                .map(|arg| &arg.token.lexeme)
                                .collect::<Vec<&String>>()
                        ))
                        .with_hint(format!(
                            "         Of which: {:?} are generic",
                            annotations
                                .iter()
                                .map(|arg| TypeId::new(&arg.token.lexeme))
                                .filter(|tid| tid.is_generic(types))
                                .map(|t| t.0)
                                .collect::<Vec<String>>()
                        )));
                    }
                } else {
                    let ann = annotations
                        .iter()
                        .map(|arg| Ok(TypeId::new(&arg.token.lexeme)))
                        .collect::<Vec<Result<TypeId, HayError>>>();
                    let gen_fn_tid = TypeId::new(&base.lexeme);
                    let func = if let Ok(func) = types
                        .get(&gen_fn_tid)
                        .unwrap_or_else(|| panic!("bad generic_fn_tid: {gen_fn_tid}"))
                        .try_generic_function(&token)
                    {
                        let map: HashMap<TypeId, TypeId> = HashMap::from_iter(
                            func.generics
                                .iter()
                                .zip(&annotations)
                                .map(|(k, v)| (k.clone(), TypeId::new(&v.token.lexeme))),
                        );

                        gen_fn_tid.assign(&token, &map, types)?
                    } else {
                        return Err(HayError::new_type_err(
                            format!(
                                "Cannot provide annotations to non generic function `{}`",
                                base.lexeme
                            ),
                            token.loc.clone(),
                        ));
                    };

                    (ann, func)
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

                sig.assign(&token, &annotations, types)?;

                if let Some(_map) = sig.evaluate(&token, stack, types)? {
                    todo!("Make a concrete version of the call")
                }

                Ok(TypedExpr::Call { func: tid.0 })
            }
            Expr::As(ExprAs {
                token,
                idents,
                block,
            }) => {
                let initial_frame = frame.clone();
                if stack.len() < idents.len() {
                    let e = HayError::new_type_err(
                        "Insufficient elements on the stack to bind",
                        token.loc,
                    )
                    .with_hint(format!(
                        "Expected {} elements to bind to idents: {:?}",
                        idents.len(),
                        idents
                            .iter()
                            .map(|arg| &arg.token.lexeme)
                            .collect::<Vec<&String>>()
                    ))
                    .with_hint(format!("Found: {:?}", stack));

                    return Err(e);
                }

                let mut typed_args = vec![];
                idents.iter().rev().for_each(|arg| {
                    let t = stack.pop().unwrap();
                    frame.push((
                        arg.token.lexeme.clone(),
                        FramedType {
                            origin: arg.token.clone(),
                            typ: t.clone(),
                            mutable: arg.mutable.is_some(),
                        },
                    ));
                    typed_args.push(t);
                });

                let mut typed_block = None;
                if let Some(blk) = block {
                    let mut tmp = vec![];
                    for e in blk {
                        tmp.push(e.type_check(
                            stack,
                            frame,
                            func,
                            global_env,
                            types,
                            generic_map,
                        )?);
                    }

                    for _ in 0..idents.len() {
                        frame.pop();
                    }

                    typed_block = Some(tmp);
                    *frame = initial_frame;
                }

                Ok(TypedExpr::As {
                    args: typed_args,
                    block: typed_block,
                })
            }
            Expr::Cast(ExprCast { token, typ }) => {
                let typ_id = TypeId::from_token(&typ, types, &vec![])?;
                let typ_id = if let Some(map) = generic_map {
                    if let Ok(tid) = typ_id.assign(&token, map, types) {
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
                                members.iter().map(|m| m.typ.clone()).collect(),
                                vec![typ_id.clone()],
                            )
                            .evaluate(&token, stack, types)?;
                            Ok(TypedExpr::Cast { typ: typ_id })
                        }
                        RecordKind::Union => {
                            let mut sigs = vec![];

                            members.iter().for_each(|m| {
                                sigs.push(Signature::new(
                                    vec![m.typ.clone()],
                                    vec![typ_id.clone()],
                                ));
                            });

                            let padding =
                                typ_id.size(types)? - stack.iter().last().unwrap().size(types)?;
                            Signature::evaluate_many(&sigs, &token, stack, types)?;

                            Ok(TypedExpr::Pad { padding })
                        }
                    },

                    Type::U64 => {
                        Signature::evaluate_many(
                            &vec![
                                Signature::new(vec![Type::U64.id()], vec![Type::U64.id()]),
                                Signature::new(vec![Type::U8.id()], vec![Type::U64.id()]),
                                Signature::new(vec![Type::Bool.id()], vec![Type::U64.id()]),
                                Signature::new(vec![Type::Char.id()], vec![Type::U64.id()]),
                                Signature::new_generic(
                                    vec![TypeId::new("*T")],
                                    vec![Type::U64.id()],
                                    vec![TypeId::new("T")],
                                ),
                                Signature::new_generic(
                                    vec![TypeId::new("&T")],
                                    vec![Type::U64.id()],
                                    vec![TypeId::new("T")],
                                ),
                            ],
                            &token,
                            stack,
                            types,
                        )?;
                        Ok(TypedExpr::Cast { typ: typ_id })
                    }
                    Type::U8 => {
                        Signature::evaluate_many(
                            &vec![
                                Signature::new(vec![Type::U64.id()], vec![Type::U8.id()]),
                                Signature::new(vec![Type::U8.id()], vec![Type::U8.id()]),
                                Signature::new(vec![Type::Bool.id()], vec![Type::U8.id()]),
                                Signature::new(vec![Type::Char.id()], vec![Type::U8.id()]),
                            ],
                            &token,
                            stack,
                            types,
                        )?;
                        Ok(TypedExpr::Cast { typ: typ_id })
                    }
                    Type::Char => {
                        Signature::evaluate_many(
                            &vec![
                                Signature::new(vec![Type::U8.id()], vec![Type::Char.id()]),
                                Signature::new(vec![Type::U64.id()], vec![Type::Char.id()]),
                                Signature::new(vec![Type::Char.id()], vec![Type::Char.id()]),
                            ],
                            &token,
                            stack,
                            types,
                        )?;
                        Ok(TypedExpr::Cast { typ: typ_id })
                    }
                    Type::Pointer { .. } => {
                        Signature::new(vec![Type::U64.id()], vec![typ_id.clone()])
                            .evaluate(&token, stack, types)?;
                        Ok(TypedExpr::Cast { typ: typ_id })
                    }
                    Type::GenericRecordBase {
                        generics, members, ..
                    } => {
                        Signature::new_generic(
                            members.iter().map(|m| m.typ.clone()).collect(),
                            vec![typ_id.clone()],
                            generics.clone(),
                        )
                        .evaluate(&token, stack, types)?;
                        Ok(TypedExpr::Cast { typ: typ_id })
                    }
                    Type::Enum { .. } => Err(HayError::new_type_err(
                        "Casting to enums is unsupported.",
                        token.loc.clone(),
                    )),
                    Type::Bool => unimplemented!(),
                    Type::GenericFunction { .. }
                    | Type::UncheckedFunction { .. }
                    | Type::Function { .. }
                    | Type::GenericRecordInstance { .. } => unreachable!(),
                    Type::Never => unreachable!("Casting to never types is not supported"),
                }
            }
            // Expr::ElseIf {
            //     else_tok,
            //     condition,
            //     block,
            // } => {
            //     let mut typed_condition = vec![];
            //     for expr in condition {
            //         typed_condition.push(expr.type_check(
            //             stack,
            //             frame,
            //             func,
            //             global_env,
            //             types,
            //             generic_map,
            //         )?);
            //     }

            //     Signature::new(vec![Type::Bool.id()], vec![]).evaluate(&else_tok, stack, types)?;

            //     let mut typed_block = vec![];
            //     for expr in block {
            //         typed_block.push(expr.type_check(
            //             stack,
            //             frame,
            //             func,
            //             global_env,
            //             types,
            //             generic_map,
            //         )?);
            //     }

            //     Ok(TypedExpr::ElseIf {
            //         condition: typed_condition,
            //         block: typed_block,
            //     })
            // }
            Expr::Ident(ExprIdent { ident }) => {
                if let Some((kind, sig)) = global_env.get(&ident.lexeme) {
                    let typed_expr = if let Some(map) = sig.evaluate(&ident, stack, types)? {
                        assert!(matches!(kind, StmtKind::Function));
                        let gen_fn_tid = TypeId::new(&ident.lexeme);
                        let monomorphised = gen_fn_tid.assign(&ident, &map, types)?;
                        Ok(TypedExpr::Call {
                            func: monomorphised.0,
                        })
                    } else {
                        match kind {
                            StmtKind::Var => Ok(TypedExpr::Global {
                                ident: ident.lexeme,
                            }),
                            StmtKind::Function => Ok(TypedExpr::Call { func: ident.lexeme }),
                        }
                    };

                    return typed_expr;
                }

                if let Some((i, (_, tid))) = frame
                    .iter()
                    .enumerate()
                    .find(|(_, (id, _))| &ident.lexeme == id)
                {
                    stack.push(tid.typ.clone());
                    return Ok(TypedExpr::Framed {
                        frame: frame.clone(),
                        idx: i,
                        inner: None,
                    });
                }

                Err(HayError::new_type_err(
                    format!("Unrecognized word `{}`", ident.lexeme),
                    ident.loc.clone(),
                ))
            }
            Expr::If(ExprIf {
                token,
                then,
                otherwise,
                finally,
            }) => {
                let sig = Signature::new(vec![Type::Bool.id()], vec![]);
                sig.evaluate(&token, stack, types)?;

                let initial_stack = stack.clone();
                let initial_frame = frame.clone();

                let mut end_stacks = vec![];

                let mut typed_then = vec![];
                let then_end_tok = match then.iter().last() {
                    Some(e) => e.token().clone(),
                    None => token.clone(),
                };
                for e in then {
                    typed_then.push(e.type_check(
                        stack,
                        frame,
                        func,
                        global_env,
                        types,
                        generic_map,
                    )?);
                }

                if !stack.contains(&Type::Never.id()) {
                    end_stacks.push((token.clone(), stack.clone()));
                }

                let mut typed_otherwise = vec![];
                for case in otherwise {
                    
                    todo!();
                    // let case_token = case.token().clone();
                    // *stack = initial_stack.clone();
                    // *frame = initial_frame.clone();

                    // typed_otherwise.push(case.type_check(
                    //     stack,
                    //     frame,
                    //     func,
                    //     global_env,
                    //     types,
                    //     generic_map,
                    // )?);

                    // if !stack.contains(&Type::Never.id()) {
                    //     end_stacks.push((case_token, stack.clone()));
                    // }
                }

                let mut typed_finally = None;
                if let Some(finally) = finally {
                    let first_tok = finally[0].token().clone();
                    *stack = initial_stack;
                    *frame = initial_frame.clone();
                    let mut tmp = vec![];
                    for e in finally {
                        tmp.push(e.type_check(
                            stack,
                            frame,
                            func,
                            global_env,
                            types,
                            generic_map,
                        )?);
                    }

                    typed_finally = Some(tmp);
                    if !stack.contains(&Type::Never.id()) {
                        end_stacks.push((first_tok, stack.clone()));
                    }
                } else {
                    *stack = initial_stack.clone();
                    end_stacks.push((then_end_tok, initial_stack));
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

                *frame = initial_frame;

                Ok(TypedExpr::If {
                    then: typed_then,
                    otherwise: typed_otherwise,
                    finally: typed_finally,
                })
            }
            Expr::Literal(ExprLiteral { literal, .. }) => {
                match &literal {
                    Literal::Bool(_) => stack.push(Type::Bool.id()),
                    Literal::Char(_) => stack.push(Type::Char.id()),
                    Literal::U64(_) => stack.push(Type::U64.id()),
                    Literal::U8(_) => stack.push(Type::U8.id()),
                    Literal::String(_) => stack.push(TypeId::new("Str")),
                }

                Ok(TypedExpr::Literal { value: literal })
            }
            Expr::Unary( ExprUnary { op: ExprOperator { op, token: op_tok }, expr }) => {
                match (&op, *expr) {
                    (Operator::Ampersand | Operator::Star, Expr::Accessor(ExprAccessor { ident, inner, .. })) => {
                        match frame
                            .iter()
                            .enumerate()
                            .find(|(_, (id, _))| &ident.lexeme == id)
                        {
                            Some((
                                idx,
                                (
                                    _,
                                    FramedType {
                                        origin,
                                        typ,
                                        mutable,
                                    },
                                ),
                            )) => {
                                if matches!((&op, mutable), (Operator::Star, false)) {
                                    return Err(
                                        HayError::new_type_err(
                                            format!("Cannot take mutable reference to immutable ident: `{}`", ident.lexeme),
                                            op_tok.loc
                                        )
                                        .with_hint_and_custom_note(
                                            format!(
                                                "Consider adding {} in binding of `{}`", 
                                                Keyword::Mut, 
                                                &ident.lexeme
                                            ), 
                                            format!("{}", &origin.loc)
                                        )
                                    );
                                }

                                let mut typ = typ;
                                for inner_member in &inner {
                                    if let Type::Record {
                                        name,
                                        members,
                                        kind,
                                        ..
                                    } = types.get(typ).unwrap()
                                    {
                                        if let Some(m) = members
                                            .iter()
                                            .find(|m| m.ident.lexeme == inner_member.lexeme)
                                        {
                                            if !m.is_public() {
                                                match &func.impl_on {
                                        Some(typ) => if &m.parent != typ {
                                            return Err(
                                                HayError::new_type_err(
                                                    format!("Cannot access {kind} `{}` member `{}` as it is declared as private.", name.lexeme, m.ident.lexeme),
                                                    op_tok.loc
                                                ).with_hint_and_custom_note(format!("{kind} `{}` declared here", name.lexeme), format!("{}", name.loc))
                                            )
                                        }
                                        _ => return Err(
                                            HayError::new_type_err(
                                                format!("Cannot access {kind} `{}` member `{}` as it is declared as private.", name.lexeme, m.ident.lexeme),
                                                op_tok.loc
                                            ).with_hint_and_custom_note(format!("{kind} `{}` declared here", name.lexeme), format!("{}", name.loc))
                                        )
                                    }
                                            }

                                            typ = &m.typ;
                                        } else {
                                            return Err(HayError::new_type_err(
                                                format!(
                                                    "{} `{}` doesn't have a member `{}`",
                                                    match kind {
                                                        RecordKind::Union => "Union",
                                                        RecordKind::Struct => "Struct",
                                                    },
                                                    name.lexeme,
                                                    inner_member.lexeme,
                                                ),
                                                op_tok.loc,
                                            )
                                            .with_hint(format!(
                                                "`{}` has the following members: {:?}",
                                                name.lexeme,
                                                members
                                                    .iter()
                                                    .map(|m| &m.ident.lexeme)
                                                    .collect::<Vec<&String>>()
                                            )));
                                        }
                                    } else {
                                        return Err(HayError::new(
                                            format!("Cannot access into non-record type `{typ}`"),
                                            op_tok.loc,
                                        ));
                                    }
                                }

                                let ptr = Type::Pointer {
                                    inner: typ.clone(),
                                    mutable: match op {
                                        Operator::Star => true,
                                        Operator::Ampersand => false,
                                        _ => unreachable!(),
                                    },
                                };
                                let ptr_tid = ptr.id();

                                types.insert(ptr_tid.clone(), ptr);
                                stack.push(ptr_tid);

                                Ok(TypedExpr::AddrFramed {
                                    frame: frame.clone(),
                                    idx,
                                    inner: if inner.is_empty() {
                                        None
                                    } else {
                                        Some(inner.iter().map(|t| t.lexeme.clone()).collect())
                                    },
                                })
                            }
                            None => Err(HayError::new_type_err(
                                format!("Can't take address of unknown identifier: `{ident}`"),
                                op_tok.loc,
                            )),
                        }
                    }
                    (Operator::Ampersand | Operator::Star, Expr::Ident (ExprIdent { ident })) => {
                        match frame
                            .iter()
                            .enumerate()
                            .find(|(_, (id, _))| &ident.lexeme == id)
                        {
                            Some((
                                idx,
                                (
                                    _,
                                    FramedType {
                                        origin,
                                        typ,
                                        mutable,
                                    },
                                ),
                            )) => {
                                if matches!((&op, mutable), (Operator::Star, false)) {
                                    return Err(
                                        HayError::new_type_err(
                                            format!("Cannot take mutable reference to immutable ident: `{}`", ident.lexeme),
                                            op_tok.loc
                                        )
                                        .with_hint_and_custom_note(
                                            format!(
                                                "Consider adding {} in binding of `{}`", 
                                                Keyword::Mut, 
                                                &ident.lexeme
                                            ), 
                                            format!("{}", &origin.loc)
                                        )
                                    );
                                }

                                let ptr = Type::Pointer {
                                    inner: typ.clone(),
                                    mutable: match op {
                                        Operator::Star => true,
                                        Operator::Ampersand => false,
                                        _ => unreachable!(),
                                    },
                                };
                                let ptr_tid = ptr.id();

                                types.insert(ptr_tid.clone(), ptr);
                                stack.push(ptr_tid);

                                Ok(TypedExpr::AddrFramed {
                                    frame: frame.clone(),
                                    idx,
                                    inner: None,
                                })
                            }
                            None => Err(HayError::new_type_err(
                                format!(
                                    "Can't take address of unknown identifier: `{}`",
                                    ident.lexeme
                                ),
                                op_tok.loc,
                            )),
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            Expr::Operator(ExprOperator { op, token: op_tok }) => match op {
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
                        Signature::new(
                            vec![Type::Char.id(), Type::Char.id()],
                            vec![Type::Char.id()],
                        ),
                    ];

                    Signature::evaluate_many(&sigs, &op_tok, stack, types)?;

                    Ok(TypedExpr::Operator {
                        op: op.clone(),
                        typ: None,
                    })
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
                        Signature::new_generic(
                            vec![TypeId::new("*T"), TypeId::new("*T")],
                            vec![Type::U64.id()],
                            vec![TypeId::new("T")],
                        ),
                    ];

                    // TODO: Comparison between pointers

                    Signature::evaluate_many(&sigs, &op_tok, stack, types)?;

                    Ok(TypedExpr::Operator {
                        op: op.clone(),
                        typ: None,
                    })
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
                        &op_tok,
                        stack,
                        types,
                    )?;

                    Ok(TypedExpr::Operator {
                        op: op.clone(),
                        typ: None,
                    })
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
                        &op_tok,
                        stack,
                        types,
                    )?;

                    Ok(TypedExpr::Operator {
                        op: op.clone(),
                        typ: None,
                    })
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

                    Signature::evaluate_many(&sigs, &op_tok, stack, types)?;

                    Ok(TypedExpr::Operator {
                        op: op.clone(),
                        typ: None,
                    })
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
                        &op_tok,
                        stack,
                        types,
                    )?;

                    Ok(TypedExpr::Operator {
                        op: op.clone(),
                        typ: None,
                    })
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
                        &op_tok,
                        stack,
                        types,
                    )?;

                    Ok(TypedExpr::Operator {
                        op: op.clone(),
                        typ: None,
                    })
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
                        &op_tok,
                        stack,
                        types,
                    )?;

                    Ok(TypedExpr::Operator {
                        op: op.clone(),
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
                        &op_tok,
                        stack,
                        types,
                    )?
                    .unwrap();

                    Ok(TypedExpr::Operator {
                        op: op.clone(),
                        typ: Some(map.get(&TypeId::new("T")).unwrap().clone()),
                    })
                }
                Operator::Write => {
                    let map = Signature::new_generic(
                        vec![TypeId::new("T"), TypeId::new("*T")],
                        vec![],
                        vec![TypeId::new("T")],
                    )
                    .evaluate(&op_tok, stack, types)?
                    .unwrap();

                    Ok(TypedExpr::Operator {
                        op: op.clone(),
                        typ: Some(map.get(&TypeId::new("T")).unwrap().clone()),
                    })
                }
                Operator::Ampersand => todo!("{op_tok}"),
                Operator::Unary { .. } => todo!("{op_tok}"),
            },
            Expr::SizeOf(ExprSizeOf { token, typ }) => {
                let tid = match TypeId::from_token(&typ, types, &vec![]) {
                    Ok(tid) => tid,
                    Err(_) => match generic_map {
                        None => {
                            return Err(HayError::new_type_err(
                                format!("Cannot get the size of unknown type {}", typ.lexeme),
                                token.loc.clone(),
                            ));
                        }
                        Some(map) => {
                            let tid = TypeId::new(&typ.lexeme);

                            if let Some(tid) = map.get(&tid) {
                                tid.clone()
                            } else {
                                return Err(HayError::new_type_err(
                                    format!("Cannot get size of unknown type: {}", typ.lexeme),
                                    token.loc.clone(),
                                ));
                            }
                        }
                    },
                };

                Signature::new(vec![], vec![Type::U64.id()]).evaluate(&token, stack, types)?;
                Ok(TypedExpr::Literal {
                    value: Literal::U64((tid.size(types)? * tid.width()) as u64),
                })
            }
            Expr::Syscall(ExprSyscall { token, n }) => {
                if stack.len() < n + 1 {
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

                Signature::new(vec![Type::U64.id()], vec![]).evaluate(&token, stack, types)?;

                for _ in 0..n {
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

                Ok(TypedExpr::Syscall { n })
            }
            Expr::Var(ExprVar { typ, ident, .. }) => {
                let typ_id = TypeId::from_token(&typ, types, &vec![])?;
                if types.get(&typ_id).is_none() {
                    return Err(HayError::new(
                        format!("Unrecognized type `{typ_id}`"),
                        typ.loc.clone(),
                    ));
                }

                let ptr = Type::Pointer {
                    inner: typ_id.clone(),
                    mutable: true,
                };
                let id = ptr.id();

                if !types.contains_key(&id) {
                    types.insert(id.clone(), ptr);
                }

                let origin = ident.clone();
                frame.push((
                    ident.lexeme,
                    FramedType {
                        origin,
                        typ: id,
                        mutable: false,
                    },
                ));

                let typ_size = typ_id.size(types)?;
                let data = if let Some((dimension, tt)) = typ.dimension()? {
                    let inner_typ = TypeId::from_type_token(&typ, &tt, types, &vec![])?;
                    Some((inner_typ.size(types)? * dimension, inner_typ.width()))
                } else {
                    None
                };

                Ok(TypedExpr::Var {
                    size: typ_size,
                    width: typ_id.width(),
                    data,
                })
            }
            Expr::While(ExprWhile { token, cond, body }) => {
                let stack_before = stack.clone();
                let frame_before = frame.clone();
                // Evaluate up to the body
                let mut typed_cond = vec![];
                for expr in cond {
                    typed_cond.push(expr.type_check(
                        stack,
                        frame,
                        func,
                        global_env,
                        types,
                        generic_map,
                    )?);
                }

                if *frame != frame_before {
                    return Err(HayError::new_type_err(
                        "Frame cannot change within the while loop condition.",
                        token.loc.clone(),
                    )
                    .with_hint(format!("Frame Before: {}", FramedType::frame_to_string(&frame_before)))
                    .with_hint(format!("Frame After : {}", FramedType::frame_to_string(frame))));
                }

                if stack.contains(&Type::Never.id()) {
                    *frame = frame_before;
                    return Ok(TypedExpr::While {
                        cond: typed_cond,
                        body: vec![],
                    });
                }

                Signature::new(vec![Type::Bool.id()], vec![]).evaluate(&token, stack, types)?;

                let mut typed_body = vec![];
                for expr in body {
                    typed_body.push(expr.type_check(
                        stack,
                        frame,
                        func,
                        global_env,
                        types,
                        generic_map,
                    )?);
                }

                if !stack.contains(&Type::Never.id())
                    && (stack.len() != stack_before.len()
                        || stack.iter().zip(&stack_before).any(|(t1, t2)| t1 != t2))
                {
                    return Err(HayError::new(
                        "While loop must not change stack between iterations.",
                        token.loc.clone(),
                    )
                    .with_hint(format!("Stack before loop: {:?}", stack_before))
                    .with_hint(format!("Stack after loop:  {:?}", stack)));
                }

                *frame = frame_before;

                Ok(TypedExpr::While {
                    cond: typed_cond,
                    body: typed_body,
                })
            }
            Expr::Return(ExprReturn { token }) => {
                let stack_expected = func
                    .outputs
                    .iter()
                    .map(|arg| &arg.typ)
                    .collect::<Vec<&TypeId>>();
                let stack_real = stack.iter().collect::<Vec<&TypeId>>();

                if stack_real != stack_expected {
                    return Err(HayError::new_type_err(
                        "Early return type check failure.",
                        token.loc,
                    )
                    .with_hint(format!(
                        "Function `{}` expects return type(s): {:?}",
                        func.name.lexeme, stack_expected
                    ))
                    .with_hint(format!("Found the following stack: {:?}", stack_real)));
                }

                stack.push(Type::Never.id());

                Ok(TypedExpr::Return)
            }
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(ExprLiteral { token,..  })
            | Expr::Operator(ExprOperator { token, .. })
            | Expr::Syscall (ExprSyscall{ token, .. })
            | Expr::Cast (ExprCast { token, .. })
            | Expr::Ident(ExprIdent { ident: token })
            | Expr::Accessor(ExprAccessor { token, .. })
            | Expr::If(ExprIf { token, .. })
            | Expr::As(ExprAs { token, .. })
            | Expr::Var(ExprVar { token, .. })
            | Expr::While(ExprWhile { token, .. })
            | Expr::SizeOf(ExprSizeOf { token, .. })
            | Expr::Return(ExprReturn { token }) => {
                write!(f, "{token}")
            }
            Expr::Unary(ExprUnary { expr, .. }) => write!(f, "{expr}"),
            Expr::AnnotatedCall(ExprAnnotatedCall {
                base, annotations, ..
            }) => {
                write!(f, "{}<", base.lexeme)?;
                for arg in &annotations[0..annotations.len() - 1] {
                    write!(f, "{} ", arg.token.lexeme)?;
                }
                write!(f, "{}>", annotations.last().unwrap().token.lexeme)
            }
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
    Global {
        ident: String,
    },
    Pad {
        padding: usize,
    },
    Return,
}

mod tests {

    #[test]
    fn bad_early_return() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "bad_early_return")
    }

    #[test]
    fn ops_after_return() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "ops_after_return")
    }

    #[test]
    fn if_no_else_modify_stack() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "if_no_else_modify_stack")
    }

    #[test]
    fn incorrect_fn_signature() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "incorrect_fn_signature")
    }

    #[test]
    fn bind_insufficient_elements() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "bind_insufficient_elements")
    }

    #[test]
    fn annotations_on_non_generic_function() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "annotations_on_non_generic_function")
    }

    #[test]
    fn enum_multiple_inner_access() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "enum_multiple_inner_access")
    }

    #[test]
    fn enum_unknown_variant() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "enum_unknown_variant")
    }

    #[test]
    fn non_record_accessor() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "non_record_accessor")
    }

    #[test]
    fn struct_accessor_without_member() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "struct_accessor_without_member")
    }

    #[test]
    fn union_accessor_without_member() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "union_accessor_without_member")
    }

    #[test]
    fn unknown_accessor_ident() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "unknown_accessor_ident")
    }

    #[test]
    fn unresolved_generics_in_annotated_call() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "unresolved_generics_in_annotated_call")
    }

    #[test]
    fn cast_u8() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "cast_u8")
    }

    #[test]
    fn cast_enum() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "cast_enum")
    }

    #[test]
    fn cast_generic_struct_instance() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "cast_generic_struct_instance")
    }

    #[test]
    fn unrecognized_ident() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "unrecognized_ident")
    }

    #[test]
    fn if_block_different_stacks() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "if_block_different_stacks")
    }

    #[test]
    fn enum_compare() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "enum_compare")
    }

    #[test]
    fn size_of_unknown_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "size_of_unknown_type")
    }
    #[test]
    fn size_of_unknown_type_generic() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "size_of_unknown_type_generic")
    }

    #[test]
    fn syscall_bad_number_of_args() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "syscall_bad_number_of_args")
    }

    #[test]
    fn syscall_wrong_sized_types() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "syscall_wrong_sized_types")
    }

    #[test]
    fn while_changes_frame() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "while_changes_frame")
    }

    #[test]
    fn while_changes_stack() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "while_changes_stack")
    }

    #[test]
    fn var_unknown_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "var_unknown_type")
    }

    #[test]
    fn address_of_unknown_ident() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "address_of_unknown_ident")
    }

    #[test]
    fn private_member_access() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "private_member_access")
    }

    #[test]
    fn private_member_access_in_impl() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "private_member_access_in_impl")
    }

    #[test]
    fn inner_address_of_no_member() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "inner_address_of_no_member")
    }

    #[test]
    fn inner_address_of_non_record_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "inner_address_of_non_record_type")
    }

    #[test]
    fn inner_address_of_private_member_in_impl() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "type_check",
            "inner_address_of_private_member_in_impl",
        )
    }

    #[test]
    fn inner_address_of_private_member() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "inner_address_of_private_member")
    }

    #[test]
    fn multiple_pointer_offsets() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "multiple_pointer_offsets")
    }

    #[test]
    fn mutable_pointer_to_immutable_framed() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "mutable_pointer_to_immutable_framed")
    }

    #[test]
    fn mutable_pointer_to_immutable_framed_inner() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "mutable_pointer_to_immutable_framed_inner")
    }

    #[test]
    fn mutable_pointer_to_immutable_fn_arg() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "mutable_pointer_to_immutable_fn_arg")
    }
}
