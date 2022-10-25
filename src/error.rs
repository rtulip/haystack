use crate::lex::token::Loc;

#[derive(Debug, Clone)]
pub enum ErrorKind {
    Error,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone, Debug)]
pub struct HayError {
    message: String,
    kind: ErrorKind,
    loc: Loc,
    hints: Vec<String>,
}

impl HayError {
    pub fn new<S>(message: S, loc: Loc) -> Self
    where
        S: Into<String>,
    {
        HayError {
            message: message.into(),
            kind: ErrorKind::Error,
            loc,
            hints: vec![],
        }
    }

    pub fn new_type_err<S>(message: S, loc: Loc) -> Self
    where
        S: Into<String>,
    {
        HayError {
            message: format!("Type Error: {}", message.into()),
            kind: ErrorKind::Error,
            loc,
            hints: vec![],
        }
    }

    pub fn with_hint<S>(self, hint: S) -> Self
    where
        S: Into<String>,
    {
        let mut hints = self.hints;
        hints.push(hint.into());
        HayError {
            message: self.message,
            kind: self.kind,
            loc: self.loc,
            hints,
        }
    }

    pub fn report(&self) {
        eprintln!("[{}] {}: {}", self.loc, self.kind, self.message);
        for hint in &self.hints {
            eprintln!("    [Note]: {hint}");
        }
    }
}
