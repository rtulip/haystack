enum TypeKind<'src, T> {
    Enum { variants: Vec<&'src str> },
    Struct { members: Vec<(&'src str, T)> },
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

    pub fn strukt<M>(name: &'src str, members: M) -> Self
    where
        M: Into<Vec<(&'src str, T)>>,
    {
        TypeDeclaration {
            name,
            kind: TypeKind::Struct {
                members: members.into(),
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
