use crate::{passes::CType, types::Type};

use super::{ExternDecl, Function, TypeDeclaration};

pub enum Stmt<'src, M, E> {
    Function(Function<'src, M, E>),
    Type(TypeDeclaration<'src, Type>),
    Extern(ExternDecl<'src>),
}

impl<'src, M, E> Stmt<'src, M, E> {
    pub fn function<F>(f: F) -> Self
    where
        F: Into<Function<'src, M, E>>,
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
        Members: Into<Vec<(&'src str, Type)>>,
    {
        Stmt::Type(TypeDeclaration::strukt(name, members))
    }

    pub fn tagged_union<V>(name: &'src str, variants: V) -> Self
    where
        V: Into<Vec<(&'src str, Type)>>,
    {
        Stmt::Type(TypeDeclaration::tagged_union(name, variants))
    }

    pub fn xtern<In, Out>(name: &'src str, input: In, output: Out) -> Self
    where
        In: Into<Vec<CType>>,
        Out: Into<CType>,
    {
        Stmt::Extern(ExternDecl::new(name, input, output))
    }
}
