use super::token::Keyword;
use crate::error::HayError;
use crate::lex::token::{Literal, Marker, Operator, Token, TokenKind};

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
            '+' => self.add_token(TokenKind::Operator(Operator::Plus)),
            '-' => {
                if self.matches('>') {
                    self.add_token(TokenKind::Marker(Marker::Arrow))
                } else {
                    self.add_token(TokenKind::Operator(Operator::Minus))
                }
            }
            '*' => self.add_token(TokenKind::Operator(Operator::Star)),
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
                } else {
                    self.add_token(TokenKind::Operator(Operator::LessThan))
                }
            }
            '>' => {
                if self.matches('=') {
                    self.add_token(TokenKind::Operator(Operator::GreaterEqual))
                } else {
                    self.add_token(TokenKind::Operator(Operator::GreaterThan))
                }
            }
            '%' => self.add_token(TokenKind::Operator(Operator::Modulo)),
            '=' => self.add_token(TokenKind::Operator(Operator::Equal)),
            '!' => {
                if self.matches('=') {
                    self.add_token(TokenKind::Operator(Operator::BangEqual))
                } else {
                    self.add_token(TokenKind::Operator(Operator::Write))
                }
            }
            '@' => self.add_token(TokenKind::Operator(Operator::Read)),
            ' ' | '\t' | '\r' => (),
            '\n' => self.newline(),
            '"' => self.string()?,
            '\'' => self.char()?,
            c => {
                if c.is_ascii_digit() {
                    self.number()?
                } else if c.is_alphabetic() {
                    self.identifier()
                } else {
                    return HayError::new(
                        format!("Unexpected character: `{c}`"),
                        &self.file,
                        self.line,
                        self.token_start,
                        self.token_end,
                    );
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
            return HayError::new(
                "Unterminated string",
                &self.file,
                self.line,
                self.token_start,
                self.token_end,
            );
        }

        self.advance();

        self.add_token(TokenKind::Literal(Literal::String(String::from(
            &self.source[self.start + 1..self.current - 1],
        ))));

        Ok(())
    }

    fn char(&mut self) -> Result<(), HayError> {
        let mut c = self.advance();
        match c {
            '\\' => match self.advance() {
                'n' => c = '\n',
                't' => c = '\t',
                'r' => c = '\r',
                c => {
                    return HayError::new(
                        format!("Unknown escaped char: `\\{c}`"),
                        &self.file,
                        self.line,
                        self.token_start,
                        self.token_end,
                    )
                }
            },
            _ => (),
        }

        if !self.matches('\'') {
            return HayError::new(
                "Unterminated character",
                &self.file,
                self.line,
                self.token_start,
                self.token_end,
            );
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
                    return HayError::new(
                        format!("Failed to convert {n} into a `u8` literal"),
                        &self.file,
                        self.line,
                        self.token_start,
                        self.token_end,
                    );
                }
            } else {
                self.add_token(TokenKind::Literal(Literal::U64(n)))
            }
        } else {
            return HayError::new(
                "Failed to parse number",
                &self.file,
                self.line,
                self.token_start,
                self.token_end,
            );
        }

        Ok(())
    }

    fn identifier(&mut self) {
        let keywords = Keyword::keywords();
        while self.peek(0).is_alphanumeric() || self.peek(0) == '_' {
            self.advance();
        }

        let ident = &self.source[self.start..self.current];
        if let Some(kind) = keywords.get(ident) {
            self.add_token(kind.clone())
        } else {
            self.add_token(TokenKind::Identifier(String::from(ident)))
        }
    }
}
