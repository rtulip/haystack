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
    hints: Vec<(String, String)>,
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
        hints.push((String::from("Note"), hint.into()));
        HayError {
            message: self.message,
            kind: self.kind,
            loc: self.loc,
            hints,
        }
    }
    pub fn with_hint_and_custom_note<S1, S2>(self, hint: S1, note: S2) -> Self
    where
        S1: Into<String>,
        S2: Into<String>,
    {
        let mut hints = self.hints;
        hints.push((note.into(), hint.into()));
        HayError {
            message: self.message,
            kind: self.kind,
            loc: self.loc,
            hints,
        }
    }

    pub fn report(&self) {
        eprintln!("[{}] {}: {}", self.loc, self.kind, self.message);
        for (note, hint) in &self.hints {
            eprintln!("    [{note}]: {hint}");
        }
    }
}
