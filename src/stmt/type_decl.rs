use std::collections::HashMap;

use crate::{
    lex::LexType,
    types::{StructType, TaggedUnionType, Type, TypeInference, TypeVar},
};

use super::TypeParameter;

#[derive(Debug)]
enum TypeKind<'src, T> {
    Enum {
        variants: Vec<&'src str>,
    },
    Struct {
        members: Vec<(&'src str, T)>,
        type_vars: Vec<TypeParameter<'src>>,
    },
    TaggedUnion {
        variants: Vec<(&'src str, T)>,
    },
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
        types: &HashMap<&'src str, (Type, Vec<TypeParameter<'src>>)>,
        free_vars: &Vec<TypeParameter<'src>>,
    ) -> Option<(Type, Vec<TypeParameter<'src>>)> {
        match self {
            LexType::Ident(id) => {
                if let Some(ty) = types.get(id) {
                    Some(ty.clone())
                } else if let Some(param) = free_vars.iter().find(|param| param.name == id) {
                    Some((Type::Var(param.tyvar.clone()), vec![]))
                } else {
                    None
                }
            }
            LexType::Pointer(ty) => {
                let (ty, params) = ty.into_type(types, free_vars)?;
                Some((Type::pointer(ty), params))
            }
            LexType::Paramaterized(ty, params) => {
                let (ty, ty_params) = ty.into_type(types, free_vars)?;

                let params: Vec<_> = params
                    .into_iter()
                    .map(|ty| ty.into_type(types, free_vars).unwrap().0)
                    .collect();

                // TODO: This doesn't forward any still-generic types. That will bite us eventually.
                Some((ty.linear_substitution(ty_params, params).ok()?, vec![]))
            }
        }
    }
}

impl<'src> TypeDeclaration<'src, LexType<'src>> {
    pub fn to_concrete(decls: Vec<Self>) -> HashMap<&'src str, (Type, Vec<TypeParameter<'src>>)> {
        let mut types = Type::builtins()
            .into_iter()
            .map(|(id, ty)| (id, (ty, vec![])))
            .collect::<HashMap<&'_ str, (Type, Vec<TypeParameter<'_>>)>>();

        for TypeDeclaration { name, kind } in decls {
            match kind {
                TypeKind::Enum { variants } => todo!(),
                TypeKind::Struct { members, type_vars } => {
                    let members = members
                        .into_iter()
                        .map(|(id, ty)| {
                            (id.to_string(), ty.into_type(&types, &type_vars).unwrap().0)
                        })
                        .collect();

                    let ty = Type::Struct(StructType {
                        name: name.to_string(),
                        members,
                    });

                    assert!(types.insert(name, (ty, type_vars)).is_none());
                }
                TypeKind::TaggedUnion { variants } => {
                    let variants = variants
                        .into_iter()
                        .map(|(id, ty)| (id.to_string(), ty.into_type(&types, &vec![]).unwrap().0))
                        .collect();

                    let ty = Type::TaggedUnion(TaggedUnionType {
                        name: name.to_string(),
                        variants,
                    });

                    assert!(types.insert(name, (ty, vec![])).is_none());
                }
            }
        }

        types
    }
}
