pub enum LexExtension<'src> {
    Ident(&'src str),
    As(Vec<&'src str>),
    Extern(&'src str, usize),
}
