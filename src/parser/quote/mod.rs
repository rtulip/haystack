use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Loc<'src> {
    file: &'src str,
    line: usize,
    start: usize,
}

impl<'src> Loc<'src> {
    pub fn new(file: &'src str, line: usize, start: usize) -> Self {
        Self { file, line, start }
    }
}

impl<'src> Display for Loc<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.start)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Quote<'src> {
    source: &'src str,
    start: usize,
    end: usize,
    loc: Loc<'src>,
}

impl<'src> Quote<'src> {
    pub fn as_str(&self) -> &str {
        &self.source[self.start..self.end]
    }

    pub fn loc(&self) -> &Loc<'src> {
        &self.loc
    }

    pub fn new(source: &'src str, start: usize, end: usize, loc: Loc<'src>) -> Self {
        Self {
            source,
            start,
            end,
            loc,
        }
    }
}

impl<'src> Display for Quote<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.loc, self.as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::{Loc, Quote};

    #[test]
    fn quote() {
        let quote = Quote::new(
            "This is an example string",
            11,
            18,
            Loc::new("filename.txt", 1, 12),
        );

        assert_eq!(format!("{quote}"), "filename.txt:1:12: example")
    }
}
