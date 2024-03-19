use std::fmt::{Debug, Display};

use crate::{
    expr::{BinOp, Expr, Literal},
    types::{Stack, Type},
};

pub enum CSsaExtension<'src> {
    BackAssign {
        input: CVar<'src>,
        output: CVar<'src>,
    },
    ExitLoop,
}

#[derive(Debug, Clone)]
pub enum CType<'src> {
    U32,
    Bool,
    U8,
    Char,
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
}

impl<'src> Display for CVar<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:?}", self.ty, self)
    }
}

impl<'src> Debug for CVar<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var{}", self.ident)
    }
}

pub struct Assignment<'src> {
    pub input: Option<Vec<CVar<'src>>>,
    pub output: Option<Vec<CVar<'src>>>,
}

impl<'src, M, E> Expr<'src, M, E> {
    pub fn into_ssa_form(self, stack: Stack) -> Expr<'src, Assignment<'src>, CSsaExtension<'src>> {
        let mut counter = 0;
        let mut stack = stack
            .into_iter()
            .map(|t| CVar::new(t.into(), &mut counter))
            .collect();

        self.get_var_ids(&mut stack, &mut counter)
    }

    fn get_var_ids(
        self,
        stack: &mut Vec<CVar<'src>>,
        counter: &mut usize,
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
                    .map(|e| e.get_var_ids(stack, counter))
                    .collect();

                // we need to be able to "return" from a block, this means that we may need to
                // create an variable _before_ the block, and then back-assign at the end of
                // the block to assign it back.
                //
                // _If_ we have no items on the stack, then no work needs to be done.
                // _If_ we have only one item on the stack, then it's easy,
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
            crate::expr::ExprBase::Ext(_) => todo!(),
        }
    }
}
