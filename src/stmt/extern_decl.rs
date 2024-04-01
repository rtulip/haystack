use crate::passes::CType;

pub struct ExternDecl<'src> {
    pub name: &'src str,
    pub input: Vec<CType>,
    pub output: CType,
}

impl<'src> ExternDecl<'src> {
    pub fn new<In, Out>(name: &'src str, input: In, output: Out) -> Self
    where
        In: Into<Vec<CType>>,
        Out: Into<CType>,
    {
        ExternDecl {
            name,
            input: input.into(),
            output: output.into(),
        }
    }
}
