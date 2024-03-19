#[derive(Debug, Clone, Copy)]
pub enum Literal<'src> {
    U32(u32),
    U8(u8),
    String(&'src str),
    Bool(bool),
}

impl<'src> From<u32> for Literal<'src> {
    fn from(val: u32) -> Literal<'src> {
        Literal::U32(val)
    }
}

impl<'src> From<&'src str> for Literal<'src> {
    fn from(val: &'src str) -> Literal<'src> {
        Literal::String(val)
    }
}

impl<'src> From<bool> for Literal<'src> {
    fn from(val: bool) -> Literal<'src> {
        Literal::Bool(val)
    }
}

impl<'src> From<u8> for Literal<'src> {
    fn from(value: u8) -> Self {
        Literal::U8(value)
    }
}
