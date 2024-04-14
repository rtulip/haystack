use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
};

use crate::{
    expr::Literal,
    types::{FunctionType, Stack, Type},
};

use super::var::CVar;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CFunction {
    pub name: String,
    pub input: Vec<CType>,
    pub output: Box<CType>,
}

impl CFunction {
    pub fn take_inputs_from_stack<T>(&self, stack: &mut Vec<T>) -> Vec<T> {
        match self.input.last() {
            Some(CType::Variadic) => stack.drain(..).collect::<Vec<_>>(),
            Some(_) => {
                assert!(!self.input.contains(&CType::Variadic));
                stack.split_off(stack.len() - self.input.len())
            }
            None => vec![],
        }
    }
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

    Function(CFunction),
    Int,
    Variadic,
}

impl CType {
    pub fn pointer(ty: CType) -> Self {
        CType::Pointer(Box::new(ty))
    }

    pub fn tuple<Ts>(ts: Ts) -> Self
    where
        Ts: Into<Vec<CType>>,
    {
        CType::Tuple(ts.into())
    }

    pub fn function<S, In, Out>(name: S, input: In, output: Out) -> Self
    where
        S: Into<String>,
        In: Into<Vec<CType>>,
        Out: Into<CType>,
    {
        CType::Function(CFunction {
            name: name.into(),
            input: input.into(),
            output: Box::new(output.into()),
        })
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
            CType::Function { .. } => todo!(),
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
            CType::Function { .. } => todo!(),
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

    pub fn expect_function(&self) -> &CFunction {
        match self {
            CType::Function(f) => f,
            _ => panic!("Expected a function!"),
        }
    }

    pub fn access_inner(&self, inner: &[&str]) -> Result<&Self, ()> {
        assert!(inner.len() >= 1);

        match self {
            CType::Struct { name, elements } => {
                if let Some((_, ty)) = elements.iter().find(|(id, _)| id == inner[0]) {
                    if inner.len() == 1 {
                        Ok(ty)
                    } else {
                        ty.access_inner(&inner[1..])
                    }
                } else {
                    Err(())
                }
            }
            _ => Err(()),
        }
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
            }
            Type::Func(_) => todo!(),
            Type::Struct(strukt) => CType::Struct {
                name: strukt.name,
                elements: strukt
                    .members
                    .into_iter()
                    .map(|(id, ty)| (id, ty.into()))
                    .collect(),
            },
            Type::Never => CType::Void,
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
            CType::Function { .. } => todo!(),
        }
    }
}
