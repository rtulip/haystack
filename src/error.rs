use crate::lex::token::Loc;

#[derive(Debug, Clone)]
pub enum ErrorKind {
    Error,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::fmt::Debug for HayError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("[{}] {}: {}", self.loc, self.kind, self.message))?;
        for (note, hint) in &self.hints {
            f.write_str(&format!("    [{note}]: {hint}"))?;
        }

        Ok(())
    }
}

#[derive(Clone)]
pub struct HayError {
    message: String,
    kind: ErrorKind,
    loc: Loc<String>,
    hints: Vec<(String, String)>,
}

impl HayError {
    pub fn new<M, L>(message: M, loc: Loc<L>) -> Self
    where
        M: Into<String>,
        L: Into<String> + PartialEq + Eq,
    {
        HayError {
            message: message.into(),
            kind: ErrorKind::Error,
            loc: Loc {
                file: loc.file.into(),
                line: loc.line,
                span: loc.span,
            },
            hints: vec![],
        }
    }

    pub fn message(self) -> String {
        self.message
    }

    pub fn new_type_err<S, L>(message: S, loc: Loc<L>) -> Self
    where
        S: Into<String>,
        L: Into<String> + PartialEq + Eq,
    {
        HayError {
            message: format!("Type Error: {}", message.into()),
            kind: ErrorKind::Error,
            loc: Loc {
                file: loc.file.into(),
                line: loc.line,
                span: loc.span,
            },
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

    pub fn optional_hint<S, T, F>(self, value: Option<T>, func: F) -> Self
    where
        S: Into<String>,
        F: FnOnce(T) -> S,
    {
        match value {
            Some(value) => self.with_hint(func(value)),
            None => self,
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
}
