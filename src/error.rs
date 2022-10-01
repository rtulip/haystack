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
    pub fn new<T, S1, S2>(
        message: S1,
        file: S2,
        line: usize,
        start: usize,
        end: usize,
    ) -> Result<T, HayError>
    where
        S1: Into<String>,
        S2: Into<String>,
    {
        Err(HayError {
            message: message.into(),
            kind: ErrorKind::Error,
            loc: Loc {
                file: file.into(),
                line,
                span: std::ops::Range { start, end },
            },
        })
    }

    pub fn report(&self) {
        eprintln!("[{}] {}: {}", self.loc, self.kind, self.message)
    }
}
