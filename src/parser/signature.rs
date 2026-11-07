use crate::parser::{argument::Argument, typ::ParseType};

#[derive(Debug)]
pub struct Signature<'src> {
    pub input: Vec<Argument<'src>>,
    pub output: Vec<ParseType<'src>>,
}
