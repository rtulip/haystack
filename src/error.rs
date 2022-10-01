use crate::lex::token::Loc;

#[derive(Debug)]
pub enum ErrorKind {
    Error,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub struct HayError {
    message: String,
    kind: ErrorKind,
    loc: Loc,
}

impl HayError {
    pub fn new<T, S>(message: S, loc: Loc) -> Result<T, HayError>
    where
        S: Into<String>,
    {
        Err(HayError {
            message: message.into(),
            kind: ErrorKind::Error,
            loc,
        })
    }

    pub fn report(&self) {
        eprintln!("[{}] {}: {}", self.loc, self.kind, self.message)
    }
}
