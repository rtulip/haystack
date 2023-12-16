use std::{collections::HashMap, fmt::Debug};

use super::{
    quote::{Loc, Quote},
    token::{Keyword, Literal, Symbol, Token, TokenKind},
};

#[derive(Clone)]
pub enum ScannerError<'src> {
    UnexpectedChar { char: char, quote: Quote<'src> },
    U32ParseError { quote: Quote<'src> },
    UnterminatedString { quote: Quote<'src> },
}

pub struct Scanner<'src> {
    file: &'src str,
    source: &'src str,
    idx: usize,
    line: usize,
    relative_idx: usize,
    token_start: usize,
}

impl<'src> Scanner<'src> {
    pub fn scan_tokens(
        file: &'src str,
        source: &'src str,
    ) -> Result<Vec<Token<'src>>, ScannerError<'src>> {
        let mut scanner = Scanner::new(file, source);
        let mut tokens = vec![];
        let keywords = Scanner::keywords();
        while !scanner.is_at_end() {
            tokens.push(scanner.next(&keywords)?);
        }

        tokens.push(scanner.build_token(TokenKind::EndOfFile));

        Ok(tokens.into_iter().rev().collect())
    }

    fn keywords() -> HashMap<&'static str, TokenKind<'static>> {
        HashMap::from([
            ("fn", Keyword::Function.into()),
            ("if", Keyword::If.into()),
            ("else", Keyword::Else.into()),
            ("as", Keyword::As.into()),
            ("enum", Keyword::Enum.into()),
        ])
    }

    fn new(file: &'src str, source: &'src str) -> Self {
        Scanner {
            file,
            source,
            idx: 0,
            line: 1,
            relative_idx: 1,
            token_start: 0,
        }
    }

    fn is_at_end(&self) -> bool {
        self.idx >= self.source.len()
    }

    fn next(
        &mut self,
        keywords: &HashMap<&'src str, TokenKind<'src>>,
    ) -> Result<Token<'src>, ScannerError<'src>> {
        let c = self.advance();

        match c {
            '(' => Ok(self.build_token(Symbol::LeftParen)),
            ')' => Ok(self.build_token(Symbol::RightParen)),
            '{' => Ok(self.build_token(Symbol::LeftBrace)),
            '}' => Ok(self.build_token(Symbol::RightBrace)),
            '[' => Ok(self.build_token(Symbol::LeftBracket)),
            ']' => Ok(self.build_token(Symbol::RightBracket)),
            '+' => Ok(self.build_token(Symbol::Plus)),
            '-' => {
                if self.peek() == '>' {
                    self.advance();
                    Ok(self.build_token(Symbol::Arrow))
                } else {
                    Ok(self.build_token(Symbol::Minus))
                }
            }
            '>' => {
                if self.peek() == '=' {
                    self.advance();
                    Ok(self.build_token(Symbol::GreaterEqual))
                } else {
                    Ok(self.build_token(Symbol::GreaterThan))
                }
            }
            '<' => {
                if self.peek() == '=' {
                    self.advance();
                    Ok(self.build_token(Symbol::LessEqual))
                } else {
                    Ok(self.build_token(Symbol::LessThan))
                }
            }
            '=' => {
                if self.peek() == '=' {
                    self.advance();
                    Ok(self.build_token(Symbol::Equals))
                } else {
                    todo!("Handle Errors")
                }
            }
            '!' => {
                if self.peek() == '=' {
                    self.advance();
                    Ok(self.build_token(Symbol::NotEqual))
                } else {
                    todo!("Add `!` token")
                }
            }
            c if c.is_ascii_digit() => self.number(),
            c if c.is_alphabetic() || c == '_' => Ok(self.ident(keywords)),
            '"' => self.string(),
            '\n' => Ok(self.newline()),
            ws if ws.is_whitespace() => Ok(self.whitespace()),
            c => Err(ScannerError::UnexpectedChar {
                char: c,
                quote: Quote::new(
                    self.source,
                    self.idx - 1,
                    self.idx,
                    Loc::new(self.file, self.line, self.relative_idx),
                ),
            }),
        }
    }

    fn build_token(&mut self, kind: impl Into<TokenKind<'src>>) -> Token<'src> {
        let tok = Token::new(
            kind.into(),
            Quote::new(
                self.source,
                self.token_start,
                self.idx,
                Loc::new(self.file, self.line, self.relative_idx),
            ),
        );

        self.relative_idx += self.idx - self.token_start;
        self.token_start = self.idx;
        tok
    }

    fn advance(&mut self) -> char {
        assert!(!self.is_at_end());
        let c = self.source.chars().nth(self.idx).unwrap();
        self.idx += 1;
        c
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source.chars().nth(self.idx).unwrap()
        }
    }

    fn whitespace(&mut self) -> Token<'src> {
        loop {
            let c = self.peek();
            if c == '\n' || !c.is_whitespace() || self.is_at_end() {
                break;
            }
            self.advance();
        }

        self.build_token(TokenKind::Whitespace)
    }

    fn newline(&mut self) -> Token<'src> {
        let t = self.build_token(TokenKind::Whitespace);
        self.line += 1;
        self.relative_idx = 1;

        t
    }

    fn number(&mut self) -> Result<Token<'src>, ScannerError<'src>> {
        while self.peek().is_ascii_digit() && !self.is_at_end() {
            self.advance();
        }

        let n = self.source[self.token_start..self.idx]
            .parse::<u32>()
            .map_err(|_| ScannerError::U32ParseError {
                quote: Quote::new(
                    self.source,
                    self.token_start,
                    self.idx,
                    Loc::new(self.file, self.line, self.relative_idx),
                ),
            })?;

        Ok(self.build_token(Literal::U32(n)))
    }

    fn ident(&mut self, keywords: &HashMap<&'src str, TokenKind<'src>>) -> Token<'src> {
        loop {
            let c = self.peek();
            if self.is_at_end() {
                break;
            }
            if c != '.' && c != '_' && !c.is_alphanumeric() {
                break;
            }

            self.advance();
        }
        let ident = &self.source[self.token_start..self.idx];
        match keywords.get(ident) {
            Some(kw) => self.build_token(kw.clone()),
            None => self.build_token(TokenKind::Identifier(ident)),
        }
    }

    fn string(&mut self) -> Result<Token<'src>, ScannerError<'src>> {
        while self.peek() != '"' && !self.is_at_end() {
            self.advance();
        }

        if self.is_at_end() {
            return Err(ScannerError::UnterminatedString {
                quote: Quote::new(
                    self.source,
                    self.token_start,
                    self.idx,
                    Loc::new(self.file, self.line, self.relative_idx),
                ),
            });
        } else {
            self.advance();
        }

        let s = &self.source[self.token_start + 1..self.idx - 1];
        Ok(self.build_token(Literal::String(s)))
    }
}

impl<'src> Debug for ScannerError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedChar { char, quote } => {
                write!(
                    f,
                    "{}: Scanner Error -- Unexpected Character: `{char}`",
                    quote.loc()
                )?;
                Ok(())
            }
            Self::U32ParseError { quote } => {
                writeln!(f, "{}: Scanner Error -- U32 Parse Error", quote.loc())?;
                writeln!(f, "  ┏━ Failed to convert `{}` into a u32", quote.as_str())?;
                writeln!(f, "  ┠─── u32 max is: {}", u32::MAX)?;
                write!(f, "  ┖─── u32 min is: {}", u32::MIN)?;
                Ok(())
            }
            Self::UnterminatedString { quote } => {
                writeln!(f, "{}: Scanner Error -- Unterminated String", quote.loc())?;
                write!(f, "  ━━ Opening quote is here: {}", quote.loc())?;
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::Scanner;

    #[test]
    fn scanner() {
        let tokens = Scanner::scan_tokens(
            "file.txt",
            "
fn dup<T>(T) -> [T T] {
    \"Hello Parser!\" putlns
    as [t] t t 
}
",
        )
        .unwrap();

        for tok in tokens {
            println!("{}: {:?}", tok.quote(), tok.kind())
        }
    }
}
