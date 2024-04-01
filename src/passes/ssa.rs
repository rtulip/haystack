use std::{
    collections::{HashMap, VecDeque},
    fmt::{Debug, Display},
};

use crate::{
    expr::{BinOp, Expr, ExprBase, Literal},
    stmt::ExternDecl,
    types::{Stack, Type, Var},
};

pub enum CSsaExtension<'src> {
    BackAssign { input: CVar, output: CVar },
    ExitLoop,
    Return(CType),
    Call(&'src str),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CType {
    U32,
    Bool,
    U8,
    Char,
    Void,
    Struct {
        name: String,
        elements: Vec<(String, CType)>,
    },
    Tuple(Vec<CType>),
    Pointer(Box<CType>),

    Int,
    Variadic,
}

impl<'src> CType {
    pub fn pointer(ty: CType) -> Self {
        CType::Pointer(Box::new(ty))
    }

    pub fn tuple<Ts>(ts: Ts) -> Self
    where
        Ts: Into<Vec<CType>>,
    {
        CType::Tuple(ts.into())
    }

    fn structs_referenced(&self) -> Option<&Self> {
        match self {
            CType::Struct { .. } | CType::Tuple(_) => Some(self),
            CType::Pointer(t) => t.structs_referenced(),
            CType::U32
            | CType::Bool
            | CType::U8
            | CType::Char
            | CType::Void
            | CType::Int
            | CType::Variadic => None,
        }
    }

    fn dependencies(&self) -> Vec<&Self> {
        match self {
            CType::U32 => vec![],
            CType::Bool => vec![],
            CType::U8 => vec![],
            CType::Char => vec![],
            CType::Void => vec![],
            CType::Int => vec![],
            CType::Variadic => vec![],
            CType::Struct { elements, .. } => elements
                .iter()
                .map(|(_, ty)| ty.structs_referenced())
                .flatten()
                .collect(),
            CType::Tuple(ts) => ts
                .iter()
                .map(|ty| ty.structs_referenced())
                .flatten()
                .collect(),
            CType::Pointer(ty) => ty.structs_referenced().into_iter().collect(),
        }
    }

    /// This is a _really_ bad topological sort which should be cleaned up at some point
    pub fn organize_dependencies<Ts>(ts: Ts) -> Result<Vec<Self>, ()>
    where
        Ts: IntoIterator<Item = CType>,
    {
        // Mark that not every node hasn't been visited
        let mut ts = ts.into_iter().map(|t| (t, false)).collect::<VecDeque<_>>();

        let mut out: Vec<CType> = vec![];
        while let Some((ty, visit)) = ts.pop_front() {
            if visit {
                return Err(());
            }

            if ty
                .dependencies()
                .iter()
                .map(|dep| {
                    if !out.contains(dep) {
                        eprintln!("{ty} depends on {dep}");
                        true
                    } else {
                        false
                    }
                })
                .any(|x| x)
            {
                // Type depends on another type, so push it back to the end, and get back to it later.
                ts.push_back((ty, true))
            } else {
                // Type has no dependencies so safe to add to the output array
                eprintln!("Added {ty}");
                out.push(ty);

                // Safe to revist, so mark that each hasn't been visited
                ts.iter_mut().for_each(|(_, visited)| *visited = false);
            }
        }

        Ok(out)
    }
}

impl<'src> From<Vec<CType>> for CType {
    fn from(mut value: Vec<CType>) -> Self {
        match value.len() {
            0 => CType::Void,
            1 => value.pop().unwrap(),
            _ => CType::Tuple(value),
        }
    }
}

impl<'src> From<Vec<CVar>> for CType {
    fn from(value: Vec<CVar>) -> Self {
        Self::from(value.into_iter().map(|var| var.ty).collect::<Vec<_>>())
    }
}

impl<'src> From<Type> for CType {
    fn from(value: Type) -> Self {
        match value {
            Type::U32 => CType::U32,
            Type::U8 => CType::U8,
            Type::Bool => CType::Bool,
            Type::Var(_) => panic!("Can't do ssa on type variables. Must do monomorphization"),
            Type::Pointer { ty, mutable } => {
                // TODO: Handle ctype pointer mutability
                CType::pointer(CType::from(*ty))
            },
            Type::Func(_) => todo!(),
            Type::Struct(strukt) => CType::Struct {
                name: strukt.name,
                elements: strukt
                    .members
                    .into_iter()
                    .map(|(id, ty)| (id, ty.into()))
                    .collect(),
            },
            Type::Never => todo!(),
        }
    }
}

impl<'src> From<Vec<Type>> for CType {
    fn from(value: Vec<Type>) -> Self {
        Self::from(
            value
                .into_iter()
                .map(|ty| CType::from(ty))
                .collect::<Vec<_>>(),
        )
    }
}

impl<'a> From<Literal<'a>> for CType {
    fn from(value: Literal) -> Self {
        match value {
            Literal::U32(_) => CType::U32,
            Literal::String(_) => CType::from(Type::string()),
            Literal::Bool(_) => CType::Bool,
            Literal::U8(_) => CType::U8,
        }
    }
}

impl<'src> Display for CType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CType::U32 => write!(f, "uint32_t"),
            CType::Bool => write!(f, "bool"),
            CType::U8 => write!(f, "uint8_t"),
            CType::Char => write!(f, "char"),
            CType::Void => write!(f, "void"),
            CType::Struct { name, .. } => write!(f, "{name}"),
            CType::Pointer(ty) => write!(f, "const {ty}*"),
            CType::Tuple(ts) => {
                write!(f, "tuple_")?;
                ts.iter()
                    .map(|ty| write!(f, "{ty}_"))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(())
            }
            CType::Int => write!(f, "int"),
            CType::Variadic => write!(f, "..."),
        }
    }
}

#[derive(Clone)]
pub struct CVar {
    pub ty: CType,
    ident: usize,
    member: Option<usize>,
}

impl CVar {
    fn new(ty: CType, counter: &mut usize) -> Self {
        let ident = *counter;
        *counter += 1;

        CVar {
            ty,
            ident,
            member: None,
        }
    }

    pub fn from_stack(stack: Stack) -> (Vec<Self>, usize) {
        let mut counter = 0;
        let stack = stack
            .into_iter()
            .map(|t| CVar::new(t.into(), &mut counter))
            .collect();

        return (stack, counter);
    }

    pub fn tuple_member(&self, idx: usize) -> Self {
        match &self.ty {
            CType::Tuple(ts) => {
                assert!(idx < ts.len(), "Out of Bounds tuple access!");

                Self {
                    ty: ts[idx].clone(),
                    ident: self.ident,
                    member: Some(idx),
                }
            }
            ty => panic!("{ty:?} is not a tuple!"),
        }
    }
}

impl<'src> Debug for CVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.ty, self)
    }
}

impl<'src> Display for CVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var{}", self.ident)?;
        if let Some(member) = &self.member {
            write!(f, ".member{member}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub input: Option<Vec<CVar>>,
    pub output: Option<Vec<CVar>>,
}

impl<'src, M, E> Expr<'src, M, E> {
    pub fn into_ssa_form(
        self,
        stack: Stack,
        env: &HashMap<Var, Type>,
        extern_fns: &HashMap<usize, ExternDecl<'src>>,
        fn_names: &'src HashMap<usize, String>,
    ) -> Expr<'src, Assignment, CSsaExtension<'src>> {
        let (mut stack, mut counter) = CVar::from_stack(stack);
        self.get_var_ids(
            &mut stack,
            &mut counter,
            env,
            extern_fns,
            fn_names,
            &mut HashMap::new(),
        )
    }

    fn get_var_ids(
        self,
        stack: &mut Vec<CVar>,
        counter: &mut usize,
        env: &HashMap<Var, Type>,
        extern_fns: &HashMap<usize, ExternDecl<'src>>,
        fn_names: &'src HashMap<usize, String>,
        bound: &mut HashMap<usize, CVar>,
    ) -> Expr<'src, Assignment, CSsaExtension<'src>> {
        match self.expr {
            crate::expr::ExprBase::Literal(lit) => {
                let var: CVar = CVar::new(lit.clone().into(), counter);
                stack.push(var.clone());
                Expr::literal(
                    lit,
                    Assignment {
                        input: None,
                        output: Some(vec![var]),
                    },
                )
            }
            crate::expr::ExprBase::Print => {
                let input = stack.pop().unwrap();
                Expr::print(Assignment {
                    input: Some(vec![input]),
                    output: None,
                })
            }
            crate::expr::ExprBase::PrintString => {
                let input = stack.pop().unwrap();
                Expr::print_string(Assignment {
                    input: Some(vec![input]),
                    output: None,
                })
            }
            crate::expr::ExprBase::Block(exprs) => {
                let mut exprs: Vec<_> = exprs
                    .into_iter()
                    .map(|e| e.get_var_ids(stack, counter, env, extern_fns, fn_names, bound))
                    .collect();

                // we need to be able to "return" from a block, this means that we may need to
                // create an variable _before_ the block, and then back-assign at the end of
                // the block to assign it back.
                //
                // _If_ we have no items on the stack, then no work needs to be done.
                // _If_ we have _multiple_ items on the stack, then we need to do multiple
                // back-assignments
                let assignment = match stack.len() {
                    0 => Assignment {
                        input: None,
                        output: None,
                    },
                    _ => Assignment {
                        input: None,
                        output: Some(
                            stack
                                .iter_mut()
                                .map(|var| {
                                    let out_var = CVar::new(var.ty.clone(), counter);
                                    exprs.push(Expr::ext(
                                        CSsaExtension::BackAssign {
                                            input: var.clone(),
                                            output: out_var.clone(),
                                        },
                                        Assignment {
                                            input: None,
                                            output: None,
                                        },
                                    ));
                                    *var = out_var.clone();
                                    out_var
                                })
                                .collect(),
                        ),
                    },
                };

                Expr::block(exprs, assignment)
            }
            crate::expr::ExprBase::BinOp(op) => match op {
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    let var: CVar = CVar::new(CType::U32, counter);
                    stack.push(var.clone());
                    Expr::binop(
                        op,
                        Assignment {
                            input: Some(vec![left, right]),
                            output: Some(vec![var]),
                        },
                    )
                }
                BinOp::Eq | BinOp::Ge | BinOp::Gt | BinOp::Le | BinOp::Lt | BinOp::Ne => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    let var: CVar = CVar::new(CType::Bool, counter);
                    stack.push(var.clone());
                    Expr::binop(
                        op,
                        Assignment {
                            input: Some(vec![left, right]),
                            output: Some(vec![var]),
                        },
                    )
                }
            },
            crate::expr::ExprBase::Call(f) => {
                let func = env
                    .get(&f)
                    .expect("call vars should be resolveable")
                    .clone()
                    .expect_function();

                let (input, output) = (func.input, func.output);

                let input = stack.split_off(stack.len() - input.len());

                // Since calling a function _can_ return multiple values,
                // things get a little tricky C-functions can only return a
                // single value, so we need to operate within that limitation.
                //
                // To get around this, functions which return multiple values
                // need to return a tuple instead, _then_ we'll back assign
                // values to destructure the tuple.

                // First, we need to generate the Vars, that will end up on the
                // stack after the function call.
                let out_vars = output
                    .into_iter()
                    .map(|ty| CVar::new(CType::from(ty), counter))
                    .collect::<Vec<_>>();

                // Then we push those vars onto the stack since they'll be
                // exposed
                stack.extend(out_vars.clone());

                // Then we need to generate the var which will be returned from
                // the function call, and create back-assignments
                let mut back_assignments = vec![];

                let call_assignment = if out_vars.len() > 0 {
                    let var = CVar::new(CType::from(out_vars.clone()), counter);

                    match &var.ty {
                        CType::Tuple(ts) => {
                            assert!(ts.len() == out_vars.len());

                            ts.iter().enumerate().zip(out_vars.iter()).for_each(
                                |((idx, _), output)| {
                                    back_assignments.push(Expr::ext(
                                        CSsaExtension::BackAssign {
                                            input: var.tuple_member(idx),
                                            output: output.clone(),
                                        },
                                        Assignment {
                                            input: None,
                                            output: None,
                                        },
                                    ))
                                },
                            );
                        }
                        _ => {
                            assert!(out_vars.len() == 1);

                            back_assignments.push(Expr::ext(
                                CSsaExtension::BackAssign {
                                    input: var.clone(),
                                    output: out_vars[0].clone(),
                                },
                                Assignment {
                                    input: None,
                                    output: None,
                                },
                            ));
                        }
                    }
                    Assignment {
                        input: Some(input),
                        output: Some(vec![var]),
                    }
                } else {
                    Assignment {
                        input: Some(input),
                        output: None,
                    }
                };

                let fn_name = fn_names
                    .get(f.func())
                    .expect("Function names should be known");

                let mut block = vec![Expr::ext(CSsaExtension::Call(fn_name), call_assignment)];

                block.extend(back_assignments);

                Expr::block(
                    block,
                    Assignment {
                        input: None,
                        output: Some(out_vars),
                    },
                )
            }

            crate::expr::ExprBase::ExternCall { extern_id, nargs } => {
                let extern_fn = extern_fns
                    .get(&extern_id)
                    .expect("call vars should be resolveable");

                let input = stack.split_off(stack.len() - nargs);

                let output = match &extern_fn.output {
                    CType::Variadic => unreachable!(),
                    CType::Void => None,
                    ty => {
                        let var = CVar::new(ty.clone(), counter);
                        stack.push(var.clone());
                        Some(vec![var])
                    }
                };

                Expr::ext(
                    CSsaExtension::Call(extern_fn.name),
                    Assignment {
                        input: Some(input),
                        output,
                    },
                )
            }
            crate::expr::ExprBase::If { then, otherwise } => {
                let condition = stack.pop().unwrap();
                assert!(condition.ty == CType::Bool);
                let mut then_stack = stack.clone();
                let mut otherwise_stack = stack.clone();
                let then =
                    then.get_var_ids(&mut then_stack, counter, env, extern_fns, fn_names, bound);
                let otherwise = otherwise.get_var_ids(
                    &mut otherwise_stack,
                    counter,
                    env,
                    extern_fns,
                    fn_names,
                    bound,
                );

                // collect the outputs into a vector (if any)
                match (&then.meta.output, &otherwise.meta.output) {
                    (None, None) => todo!(),
                    (None, Some(_)) | (Some(_), None) => {
                        unreachable!("If branches should product the same output")
                    }
                    (Some(then_out), Some(otherwise_out)) => {
                        assert!(then_out.len() == otherwise_out.len());

                        let mut output = vec![];
                        let mut then_back_assign = vec![];
                        let mut otherwise_back_assign = vec![];

                        then_out
                            .iter()
                            .zip(otherwise_out.iter())
                            .for_each(|(then, otherwise)| {
                                assert!(then.ty == otherwise.ty);

                                let out = CVar::new(then.ty.clone(), counter);
                                then_back_assign.push(Expr::ext(
                                    CSsaExtension::BackAssign {
                                        input: then.clone(),
                                        output: out.clone(),
                                    },
                                    Assignment {
                                        input: None,
                                        output: None,
                                    },
                                ));
                                otherwise_back_assign.push(Expr::ext(
                                    CSsaExtension::BackAssign {
                                        input: otherwise.clone(),
                                        output: out.clone(),
                                    },
                                    Assignment {
                                        input: None,
                                        output: None,
                                    },
                                ));
                                output.push(out);
                            });

                        let mut then_block = vec![then];
                        let mut otherwise_block = vec![otherwise];
                        then_block.extend(then_back_assign);
                        otherwise_block.extend(otherwise_back_assign);

                        stack.extend(output.clone());

                        Expr::iff(
                            Expr::block(
                                then_block,
                                Assignment {
                                    input: None,
                                    output: None,
                                },
                            ),
                            Expr::block(
                                otherwise_block,
                                Assignment {
                                    input: None,
                                    output: None,
                                },
                            ),
                            Assignment {
                                input: Some(vec![condition]),
                                output: Some(output),
                            },
                        )
                    }
                }
            }
            crate::expr::ExprBase::While { condition, body } => {
                let before = stack.clone();

                let mut condition: Vec<_> = condition
                    .into_iter()
                    .map(|e| e.get_var_ids(stack, counter, env, extern_fns, fn_names, bound))
                    .collect();

                let var = stack.pop().unwrap();

                condition.push(Expr::ext(
                    CSsaExtension::ExitLoop,
                    Assignment {
                        input: Some(vec![var]),
                        output: None,
                    },
                ));

                let mut body = body.get_var_ids(stack, counter, env, extern_fns, fn_names, bound);

                assert!(before.len() == stack.len());
                match &mut body.expr {
                    ExprBase::Block(ref mut exprs) => {
                        before.iter().zip(stack.iter()).for_each(|(before, after)| {
                            assert!(before.ty == after.ty);
                            exprs.push(Expr::ext(
                                CSsaExtension::BackAssign {
                                    input: after.clone(),
                                    output: before.clone(),
                                },
                                Assignment {
                                    input: None,
                                    output: None,
                                },
                            ));
                        });
                    }
                    _ => todo!(),
                };

                *stack = before;

                Expr::wile(
                    condition,
                    body,
                    Assignment {
                        input: None,
                        output: None,
                    },
                )
            }
            crate::expr::ExprBase::Bind(n) => {
                assert!(stack.len() <= n);

                let popped = stack.split_off(stack.len() - n);
                popped.into_iter().for_each(|var| {
                    let id = bound.len();
                    bound.insert(id, var);
                });

                Expr::bind(
                    n,
                    Assignment {
                        input: None,
                        output: None,
                    },
                )
            }
            crate::expr::ExprBase::Var(Var::Ident(id)) => {
                let ref_var = bound.get(&id).expect("Bindings should be known");
                let var: CVar = CVar::new(ref_var.ty.clone(), counter);
                stack.push(var.clone());
                Expr::var(
                    Var::Ident(id),
                    Assignment {
                        input: Some(vec![ref_var.clone()]),
                        output: Some(vec![var]),
                    },
                )
            }
            crate::expr::ExprBase::Var(Var::Func(_)) => unreachable!(),
            crate::expr::ExprBase::Ext(_) => todo!(),
        }
    }
}
