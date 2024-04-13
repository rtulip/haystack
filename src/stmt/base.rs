use crate::{c::CType, types::Type};

use super::{Function, TypeDeclaration};

pub enum Stmt<'src, T, M, E> {
    Function(Function<'src, T, M, E>),
    Type(TypeDeclaration<'src, T>),
    Extern(CType),
}

impl<'src, T, M, E> Stmt<'src, T, M, E> {
    pub fn function<F>(f: F) -> Self
    where
        F: Into<Function<'src, T, M, E>>,
    {
        Stmt::Function(f.into())
    }

    pub fn enumm<V>(name: &'src str, variants: V) -> Self
    where
        V: Into<Vec<&'src str>>,
    {
        Stmt::Type(TypeDeclaration::enumm(name, variants))
    }

    pub fn strukt<Members>(name: &'src str, members: Members) -> Self
    where
        Members: Into<Vec<(&'src str, T)>>,
    {
        Stmt::Type(TypeDeclaration::strukt(name, members))
    }

    pub fn tagged_union<V>(name: &'src str, variants: V) -> Self
    where
        V: Into<Vec<(&'src str, T)>>,
    {
        Stmt::Type(TypeDeclaration::tagged_union(name, variants))
    }

    pub fn xtern<In, Out>(name: &str, input: In, output: Out) -> Self
    where
        In: Into<Vec<CType>>,
        Out: Into<CType>,
    {
        Stmt::Extern(CType::function(name, input, output))
    }
}
