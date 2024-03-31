use crate::passes::CType;

pub struct ExternDecl<'src> {
    pub name: &'src str,
    pub input: Vec<CType<'src>>,
    pub output: CType<'src>,
}

impl<'src> ExternDecl<'src> {
    pub fn new<In, Out>(name: &'src str, input: In, output: Out) -> Self
    where
        In: Into<Vec<CType<'src>>>,
        Out: Into<CType<'src>>,
    {
        ExternDecl {
            name,
            input: input.into(),
            output: output.into(),
        }
    }
}
