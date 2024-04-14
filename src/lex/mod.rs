pub enum LexExtension<'src> {
    Ident(&'src str),
    As(Vec<&'src str>),
    IdentSeq(Vec<&'src str>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum LexType<'src> {
    Ident(&'src str),
    Pointer(Box<Self>),
}

impl<'src> LexType<'src> {
    pub fn pointer(ty: Self) -> Self {
        LexType::Pointer(Box::new(ty))
    }
}
