use std::fmt::{Debug, Display};

use crate::{
    expr::{Expr, Literal},
    types::{Stack, Type},
};

pub struct Reassignment<'src> {
    input: CVar<'src>,
    output: CVar<'src>,
}

pub enum CSsaExtension<'src> {
    BackAssign(Vec<Reassignment<'src>>),
    ExitLoop,
}

#[derive(Debug, Clone)]
pub enum CType<'src> {
    U32,
    Bool,
    U8,
    Struct{
        name: &'src str,
        elements: Vec<(&'src str, CType<'src>)>,
    },
    Pointer(Box<CType<'src>>)
}

impl<'src> CType<'src> {
    pub fn pointer(ty: CType<'src>) -> Self {
        CType::Pointer(Box::new(ty))
    }

    pub fn string() -> Self {
        CType::Struct { name: "HaystackStr", elements: vec![
            ("size", CType::U32),
            ("string", CType::pointer(CType::U32))
        ] }
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
            CType::Struct { name, ..} => write!(f, "{name}"),
            CType::Pointer(ty) => write!(f, "{ty}*"),
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
    pub output: Option<CVar<'src>>,
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
                        output: Some(var),
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
                let exprs: Vec<_> = exprs
                    .into_iter()
                    .map(|e| e.get_var_ids(stack, counter))
                    .collect();
                Expr::block(
                    exprs,
                    Assignment {
                        input: None,
                        output: None,
                    },
                )
            }
            crate::expr::ExprBase::Ext(_) => todo!(),
        }
    }
}
