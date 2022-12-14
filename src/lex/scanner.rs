use super::token::Keyword;
use crate::error::HayError;
use crate::lex::token::{Literal, Loc, Marker, Operator, Token, TokenKind};

pub struct Scanner {
    file: String,
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    token_start: usize,
    token_end: usize,
}

impl Scanner {
    pub fn new<S>(file: S, source: S) -> Self
    where
        S: Into<String>,
    {
        Scanner {
            file: file.into(),
            source: source.into(),
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
            token_start: 1,
            token_end: 1,
        }
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token>, HayError> {
        while !self.is_at_end() {
            self.start = self.current;
            self.token_start = self.token_end;
            self.scan_token()?;
        }

        self.tokens.push(Token::new(
            TokenKind::EoF,
            "",
            &self.file,
            self.line,
            self.token_start,
            self.token_end,
        ));

        Ok(self.tokens)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.token_end += 1;

        self.source.chars().nth(self.current - 1).unwrap()
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.peek(0) != expected {
            false
        } else {
            self.current += 1;
            self.token_end += 1;
            true
        }
    }

    fn peek(&self, offset: usize) -> char {
        self.source
            .chars()
            .nth(self.current + offset)
            .unwrap_or('\0')
    }

    fn newline(&mut self) {
        self.line += 1;
        self.token_start = 1;
        self.token_end = 1;
    }

    fn add_token(&mut self, kind: TokenKind) {
        self.tokens.push(Token::new(
            kind,
            &self.source[self.start..self.current],
            &self.file,
            self.line,
            self.token_start,
            self.token_end,
        ));
    }

    fn scan_token(&mut self) -> Result<(), HayError> {
        let c = self.advance();
        match c {
            '(' => self.add_token(TokenKind::Marker(Marker::LeftParen)),
            ')' => self.add_token(TokenKind::Marker(Marker::RightParen)),
            '{' => self.add_token(TokenKind::Marker(Marker::LeftBrace)),
            '}' => self.add_token(TokenKind::Marker(Marker::RightBrace)),
            '[' => self.add_token(TokenKind::Marker(Marker::LeftBracket)),
            ']' => self.add_token(TokenKind::Marker(Marker::RightBracket)),
            ':' => {
                if self.matches(':') {
                    self.add_token(TokenKind::Marker(Marker::DoubleColon))
                } else {
                    self.add_token(TokenKind::Marker(Marker::Colon))
                }
            }
            '+' => {
                if self.peek(0).is_alphabetic() {
                    self.identifier()?
                } else {
                    self.add_token(TokenKind::Operator(Operator::Plus));
                }
            }
            '-' => {
                if self.matches('>') {
                    self.add_token(TokenKind::Marker(Marker::Arrow))
                } else if self.peek(0).is_alphabetic() {
                    self.identifier()?
                } else {
                    self.add_token(TokenKind::Operator(Operator::Minus))
                }
            }
            '*' => {
                let c = self.peek(0);
                if c.is_alphabetic() || c == '*' || c == '&' {
                    let tok = Token::new(
                        TokenKind::Operator(Operator::Star),
                        &self.source[self.start..self.current],
                        &self.file,
                        self.line,
                        self.token_start,
                        self.token_end,
                    );

                    self.add_token(TokenKind::Operator(Operator::Unary(Box::new(tok))));
                } else {
                    self.add_token(TokenKind::Operator(Operator::Star))
                }
            }
            '/' => {
                if self.matches('/') {
                    // skip the comment
                    while self.peek(0) != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenKind::Operator(Operator::Slash))
                }
            }
            '<' => {
                if self.matches('=') {
                    self.add_token(TokenKind::Operator(Operator::LessEqual))
                } else if self.matches('<') {
                    self.add_token(TokenKind::Operator(Operator::ShiftLeft))
                } else {
                    self.add_token(TokenKind::Operator(Operator::LessThan))
                }
            }
            '>' => {
                if self.matches('=') {
                    self.add_token(TokenKind::Operator(Operator::GreaterEqual))
                } else if self.matches('>') {
                    self.add_token(TokenKind::Operator(Operator::ShiftRight))
                } else {
                    self.add_token(TokenKind::Operator(Operator::GreaterThan))
                }
            }
            '%' => self.add_token(TokenKind::Operator(Operator::Modulo)),
            '=' => {
                if self.matches('=') {
                    self.add_token(TokenKind::Operator(Operator::Equal))
                } else {
                    return Err(HayError::new(
                        format!("Unrecognized token: `={}`", self.peek(0)),
                        Loc::new(&self.file, self.line, self.token_start, self.token_end),
                    ));
                }
            }
            '!' => {
                if self.matches('=') {
                    self.add_token(TokenKind::Operator(Operator::BangEqual))
                } else {
                    self.add_token(TokenKind::Operator(Operator::Write))
                }
            }
            '@' => self.add_token(TokenKind::Operator(Operator::Read)),
            '&' => {
                let c = self.peek(0);
                if c.is_alphabetic() || c == '*' || c == '&' {
                    let tok = Token::new(
                        TokenKind::Operator(Operator::Ampersand),
                        &self.source[self.start..self.current],
                        &self.file,
                        self.line,
                        self.token_start,
                        self.token_end,
                    );
                    self.add_token(TokenKind::Operator(Operator::Unary(Box::new(tok))))
                } else {
                    self.add_token(TokenKind::Operator(Operator::Ampersand))
                }
            }
            '|' => self.add_token(TokenKind::Operator(Operator::Pipe)),
            '^' => self.add_token(TokenKind::Operator(Operator::Caret)),
            ' ' | '\t' | '\r' => (),
            '\n' => self.newline(),
            '"' => self.string()?,
            '\'' => self.char()?,
            c => {
                if c.is_ascii_digit() {
                    self.number()?
                } else if c.is_alphabetic() || c == '_' {
                    self.identifier()?
                } else {
                    return Err(HayError::new(
                        format!("Unexpected character: `{c}`"),
                        Loc::new(&self.file, self.line, self.token_start, self.token_end),
                    ));
                }
            }
        }

        Ok(())
    }

    fn string(&mut self) -> Result<(), HayError> {
        while self.peek(0) != '"' && !self.is_at_end() {
            if self.peek(0) == '\n' {
                self.newline()
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(HayError::new(
                "Unterminated string",
                Loc::new(&self.file, self.line, self.token_start, self.token_end),
            ));
        }

        self.advance();

        self.add_token(TokenKind::Literal(Literal::String(String::from(
            &self.source[self.start + 1..self.current - 1],
        ))));

        Ok(())
    }

    fn char(&mut self) -> Result<(), HayError> {
        let mut c = self.advance();

        if c == '\\' {
            match self.advance() {
                'n' => c = '\n',
                't' => c = '\t',
                'r' => c = '\r',
                '0' => c = '\0',
                '\'' => c = '\'',
                '\\' => c = '\\',
                c => {
                    return Err(HayError::new(
                        format!("Unknown escaped character: `\\{c}`"),
                        Loc::new(&self.file, self.line, self.token_start, self.token_end),
                    ))
                }
            }
        }

        if !self.matches('\'') {
            return Err(HayError::new(
                "Unterminated character",
                Loc::new(&self.file, self.line, self.token_start, self.token_end),
            ));
        }

        self.add_token(TokenKind::Literal(Literal::Char(c)));

        Ok(())
    }

    fn number(&mut self) -> Result<(), HayError> {
        while self.peek(0).is_ascii_digit() {
            self.advance();
        }

        if let Ok(n) = String::from(&self.source[self.start..self.current]).parse::<u64>() {
            if self.peek(0) == 'u' && self.peek(1) == '8' {
                self.advance();
                self.advance();
                if let Ok(n) = u8::try_from(n) {
                    self.add_token(TokenKind::Literal(Literal::U8(n)))
                } else {
                    return Err(HayError::new(
                        format!("Failed to convert {n} into a `u8` literal"),
                        Loc::new(&self.file, self.line, self.token_start, self.token_end),
                    ));
                }
            } else {
                self.add_token(TokenKind::Literal(Literal::U64(n)))
            }
        } else {
            return Err(HayError::new(
                "Failed to parse number",
                Loc::new(&self.file, self.line, self.token_start, self.token_end),
            ));
        }

        Ok(())
    }

    fn identifier(&mut self) -> Result<(), HayError> {
        let keywords = Keyword::keywords();
        while ![
            ' ', '\n', '\t', '\r', ':', '{', '}', '[', ']', '(', ')', '<', '>',
        ]
        .contains(&self.peek(0))
            && !self.is_at_end()
        {
            self.advance();
        }

        let ident = &self.source[self.start..self.current];
        if let Some(kind) = keywords.get(ident) {
            match kind {
                TokenKind::Syscall(_) => self.syscall()?,
                _ => self.add_token(kind.clone()),
            }
        } else {
            self.add_token(TokenKind::Ident(String::from(ident)))
        }

        Ok(())
    }

    fn syscall(&mut self) -> Result<(), HayError> {
        if !self.matches('(') {
            return Err(HayError::new(
                format!("Expected {} after `syscall`.", Marker::LeftParen),
                Loc::new(&self.file, self.line, self.token_start, self.token_end),
            ));
        }

        let n = self.advance();
        let n = match String::from(n).parse::<usize>() {
            Ok(n) => {
                if n > 0 && n < 7 {
                    n
                } else {
                    return Err(HayError::new(
                        format!("Expected `1..6` after `syscall(`, but found {n}"),
                        Loc::new(&self.file, self.line, self.token_start, self.token_end),
                    ));
                }
            }
            Err(_) => {
                return Err(HayError::new(
                    "Expected `1..6` after `syscall(`.",
                    Loc::new(&self.file, self.line, self.token_start, self.token_end),
                ))
            }
        };

        if !self.matches(')') {
            return Err(HayError::new(
                format!("Expected {} after `syscall`.", Marker::RightParen),
                Loc::new(&self.file, self.line, self.token_start, self.token_end),
            ));
        }

        self.add_token(TokenKind::Syscall(n));
        Ok(())
    }
}

mod tests {

    #[test]
    fn scan_good_escape_chars() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/scanner", "scan_good_escape_chars", None)
    }

    #[test]
    fn scan_bad_equals_operator() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/scanner", "scan_bad_equals_operator", None)
    }

    #[test]
    fn scan_multi_line_string() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/scanner", "scan_multi_line_string", None)
    }

    #[test]
    fn scan_bad_syscall_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/scanner", "scan_bad_syscall_open", None)
    }

    #[test]
    fn scan_bad_syscall_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/scanner", "scan_bad_syscall_close", None)
    }

    #[test]
    fn scan_bad_syscall_number_too_large() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/scanner",
            "scan_bad_syscall_number_too_large",
            None,
        )
    }

    #[test]
    fn scan_bad_syscall_number_too_small() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/scanner",
            "scan_bad_syscall_number_too_small",
            None,
        )
    }

    #[test]
    fn scan_bad_syscall_parameter() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test(
            "src/tests/scanner",
            "scan_bad_syscall_parameter",
            None,
        )
    }

    #[test]
    fn scan_unexpected_char() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/scanner", "scan_unexpected_char", None)
    }

    #[test]
    fn scan_bad_number_literal() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/scanner", "scan_bad_number_literal", None)
    }

    #[test]
    fn scan_bad_u8() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/scanner", "scan_bad_u8", None)
    }

    #[test]
    fn scan_unterminated_char() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/scanner", "scan_unterminated_char", None)
    }

    #[test]
    fn scan_unterminated_string() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/scanner", "scan_unterminated_string", None)
    }

    #[test]
    fn scan_bad_escaped_char() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/scanner", "scan_bad_escaped_char", None)
    }

    #[test]
    fn scan_constructor_and_destructor() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("src/tests/scanner", "scan_special_functions", None)
    }
}
