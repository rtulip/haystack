use std::collections::HashMap;

use crate::{
    lex::LexType,
    types::{StructType, TaggedUnionType, Type, TypeVar},
};

use super::TypeParameter;

#[derive(Debug)]
enum TypeKind<'src, T> {
    Enum { variants: Vec<&'src str> },
    Struct { members: Vec<(&'src str, T)>, type_vars: Vec<TypeParameter<'src>>, },
    TaggedUnion { variants: Vec<(&'src str, T)> },
}

pub struct TypeDeclaration<'src, T> {
    pub name: &'src str,
    kind: TypeKind<'src, T>,
}

impl<'src, T> TypeDeclaration<'src, T> {
    pub fn enumm<V>(name: &'src str, variants: V) -> Self
    where
        V: Into<Vec<&'src str>>,
    {
        TypeDeclaration {
            name,
            kind: TypeKind::Enum {
                variants: variants.into(),
            },
        }
    }

    pub fn strukt<M>(name: &'src str, members: M, type_vars: Vec<TypeParameter<'src>>) -> Self
    where
        M: Into<Vec<(&'src str, T)>>,
    {
        TypeDeclaration {
            name,
            kind: TypeKind::Struct {
                members: members.into(),
                type_vars,
            },
        }
    }

    pub fn tagged_union<V>(name: &'src str, variants: V) -> Self
    where
        V: Into<Vec<(&'src str, T)>>,
    {
        TypeDeclaration {
            name,
            kind: TypeKind::TaggedUnion {
                variants: variants.into(),
            },
        }
    }
}

impl<'src> LexType<'src> {
    pub fn into_type(
        self,
        types: &HashMap<&'src str, Type>,
        free_vars: &HashMap<&'src str, TypeVar>,
    ) -> Option<Type> {
        match self {
            LexType::Ident(id) => {
                if let Some(ty) = types.get(id) {
                    Some(ty.clone())
                } else if let Some(tyvar) = free_vars.get(id) {
                    Some(Type::Var(tyvar.clone()))
                } else {
                    None
                }
            }
            LexType::Pointer(ty) => Some(Type::pointer(ty.into_type(types, free_vars)?)),
        }
    }
}

impl<'src> TypeDeclaration<'src, LexType<'src>> {
    pub fn to_concrete(decls: Vec<Self>) -> HashMap<&'src str, Type> {
        let mut types = Type::builtins();

        for TypeDeclaration { name, kind } in decls {
            match kind {
                TypeKind::Enum { variants } => todo!(),
                TypeKind::Struct { members, type_vars } => {
                    let free_vars = HashMap::from_iter(type_vars.into_iter().map(|param| (param.name, param.tyvar)));
                    
                    let members = members
                        .into_iter()
                        .map(|(id, ty)| {
                            (
                                id.to_string(),
                                ty.into_type(&types, &free_vars).unwrap(),
                            )
                        })
                        .collect();

                    let ty = Type::Struct(StructType {
                        name: name.to_string(),
                        members,
                    });

                    assert!(types.insert(name, ty).is_none());
                }
                TypeKind::TaggedUnion { variants } => {
                    let variants = variants
                        .into_iter()
                        .map(|(id, ty)| {
                            (
                                id.to_string(),
                                ty.into_type(&types, &HashMap::new()).unwrap(),
                            )
                        })
                        .collect();

                    let ty = Type::TaggedUnion(TaggedUnionType {
                        name: name.to_string(),
                        variants,
                    });

                    assert!(types.insert(name, ty).is_none());
                }
            }
        }

        types
    }
}
