use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use crate::{
    expr::{BinOp, Expr, Literal},
    types::{Stack, Type, Var},
};

pub enum CSsaExtension<'src> {
    BackAssign {
        input: CVar<'src>,
        output: CVar<'src>,
    },
    ExitLoop,
    Return(CType<'src>),
    Call(&'src str)
}

#[derive(Debug, Clone)]
pub enum CType<'src> {
    U32,
    Bool,
    U8,
    Char,
    Void,
    Struct {
        name: &'src str,
        elements: Vec<(&'src str, CType<'src>)>,
    },
    Tuple(Vec<CType<'src>>),
    Pointer(Box<CType<'src>>),
}

impl<'src> CType<'src> {
    pub fn pointer(ty: CType<'src>) -> Self {
        CType::Pointer(Box::new(ty))
    }

    pub fn string() -> Self {
        CType::Struct {
            name: "Str",
            elements: vec![
                ("size", CType::U32),
                ("string", CType::pointer(CType::Char)),
            ],
        }
    }

    pub fn tuple<Ts>(ts: Ts) -> Self
    where
        Ts: Into<Vec<CType<'src>>>,
    {
        CType::Tuple(ts.into())
    }
}

impl<'src> From<Vec<CType<'src>>> for CType<'src> {
    fn from(mut value: Vec<CType<'src>>) -> Self {
        match value.len() {
            0 => CType::Void,
            1 => value.pop().unwrap(),
            _ => CType::Tuple(value),
        }
    }
}

impl<'src> From<Type> for CType<'src> {
    fn from(value: Type) -> Self {
        match value {
            Type::U32 => CType::U32,
            Type::U8 => CType::U8,
            Type::Bool => CType::Bool,
            Type::Var(_) => todo!(),
            Type::Pointer { ty, mutable } => todo!(),
            Type::Func { input, output } => todo!(),
            Type::String => todo!(),
            Type::Never => todo!(),
        }
    }
}

impl<'src> From<Vec<Type>> for CType<'src> {
    fn from(value: Vec<Type>) -> Self {
        Self::from(
            value
                .into_iter()
                .map(|ty| CType::from(ty))
                .collect::<Vec<_>>(),
        )
    }
}

impl<'a, 'b> From<Literal<'a>> for CType<'b> {
    fn from(value: Literal) -> Self {
        match value {
            Literal::U32(_) => CType::U32,
            Literal::String(_) => CType::string(),
            Literal::Bool(_) => CType::Bool,
            Literal::U8(_) => CType::U8,
        }
    }
}

impl<'src> Display for CType<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CType::U32 => write!(f, "uint32_t"),
            CType::Bool => write!(f, "bool"),
            CType::U8 => write!(f, "uint8_t"),
            CType::Char => write!(f, "char"),
            CType::Void => write!(f, "void"),
            CType::Struct { name, .. } => write!(f, "{name}"),
            CType::Pointer(ty) => write!(f, "{ty}*"),
            CType::Tuple(ts) => {
                write!(f, "struct {{ ")?;
                ts.iter()
                    .enumerate()
                    .map(|(id, ty)| write!(f, "{ty} id{id}; "))
                    .collect::<Result<Vec<_>, _>>()?;
                write!(f, "}}")
            }
        }
    }
}

#[derive(Clone)]
pub struct CVar<'src> {
    ty: CType<'src>,
    ident: usize,
}

impl<'src> CVar<'src> {
    fn new(ty: CType<'src>, counter: &mut usize) -> Self {
        let ident = *counter;
        *counter += 1;

        CVar { ty, ident }
    }

    pub fn from_stack(stack: Stack) -> (Vec<Self>, usize) {
        let mut counter = 0;
        let mut stack = stack
            .into_iter()
            .map(|t| CVar::new(t.into(), &mut counter))
            .collect();

        return (stack, counter);
    }
}

impl<'src> Debug for CVar<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.ty, self)
    }
}

impl<'src> Display for CVar<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var{}", self.ident)
    }
}

#[derive(Debug, Clone)]
pub struct Assignment<'src> {
    pub input: Option<Vec<CVar<'src>>>,
    pub output: Option<Vec<CVar<'src>>>,
}

impl<'src, M, E> Expr<'src, M, E> {
    pub fn into_ssa_form(
        self,
        stack: Stack,
        env: &HashMap<Var, Type>,
        fn_names: &'src HashMap<usize, String>,
    ) -> Expr<'src, Assignment<'src>, CSsaExtension<'src>> {
        let (mut stack, mut counter) = CVar::from_stack(stack);
        self.get_var_ids(&mut stack, &mut counter, env, fn_names)
    }

    fn get_var_ids(
        self,
        stack: &mut Vec<CVar<'src>>,
        counter: &mut usize,
        env: &HashMap<Var, Type>,
        fn_names: &'src HashMap<usize, String>,
    ) -> Expr<'src, Assignment<'src>, CSsaExtension<'src>> {
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
                    .map(|e| e.get_var_ids(stack, counter, env, fn_names))
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
                _ => todo!(),
            },
            crate::expr::ExprBase::Call(f) => {
                let (input, output) = env
                    .get(&f)
                    .expect("call vars should be resolveable")
                    .clone()
                    .expect_function();

                let input = stack.split_off(stack.len() - input.len());

                assert!(output.len() < 2, "multiple return not supported yet");

                let output = CVar::new(CType::from(output), counter);
                stack.push(output.clone());
            
                let fn_name = fn_names.get(f.func()).expect("Function names should be known");

                Expr::ext(
                    CSsaExtension::Call(fn_name),
                    Assignment {
                        input: Some(input),
                        output: Some(vec![output]),
                    },
                )
            }

            crate::expr::ExprBase::Ext(_) => todo!(),
        }
    }
}
