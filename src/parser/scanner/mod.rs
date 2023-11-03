use super::{
    quote::{Loc, Quote},
    token::{Symbol, Token, TokenKind},
};

struct Scanner<'src> {
    file: &'src str,
    source: &'src str,
    idx: usize,
    line: usize,
    relative_idx: usize,
    token_start: usize,
}

impl<'src> Scanner<'src> {
    pub fn scan_tokens(file: &'src str, source: &'src str) -> Vec<Token<'src>> {
        let mut scanner = Scanner::new(file, source);
        let mut tokens = vec![];
        while !scanner.is_at_end() {
            tokens.push(scanner.next());
        }

        tokens
    }

    fn new(file: &'src str, source: &'src str) -> Self {
        Scanner {
            file,
            source,
            idx: 0,
            line: 1,
            relative_idx: 0,
            token_start: 0,
        }
    }

    fn is_at_end(&self) -> bool {
        self.idx >= self.source.len()
    }

    fn next(&mut self) -> Token<'src> {
        let c = self.advance();

        match c {
            '(' => self.build_token(Symbol::LeftParen),
            ')' => self.build_token(Symbol::RightParen),
            '{' => self.build_token(Symbol::LeftBrace),
            '}' => self.build_token(Symbol::RightBrace),
            '[' => self.build_token(Symbol::LeftBracket),
            ']' => self.build_token(Symbol::RightBracket),
            '+' => self.build_token(Symbol::Plus),
            '-' => self.build_token(Symbol::Minus),
            '>' => {
                if self.peek() == '=' {
                    self.advance();
                    self.build_token(Symbol::GreaterEqual)
                } else {
                    self.build_token(Symbol::GreaterThan)
                }
            }
            '<' => {
                if self.peek() == '=' {
                    self.advance();
                    self.build_token(Symbol::LessEqual)
                } else {
                    self.build_token(Symbol::LessThan)
                }
            }
            '\n' => self.newline(),
            ws if ws.is_whitespace() => self.whitespace(),
            c => panic!("Not sure what to do with `{c}` yet..."),
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

        self.token_start = self.idx;
        tok
    }

    fn advance(&mut self) -> char {
        assert!(!self.is_at_end());
        let c = self.source.chars().nth(self.idx).unwrap();
        self.idx += 1;
        self.relative_idx += 1;
        c
    }

    fn peek(&self) -> char {
        assert!(!self.is_at_end());
        self.source.chars().nth(self.idx).unwrap()
    }

    fn whitespace(&mut self) -> Token<'src> {
        loop {
            let c = self.peek();
            if c == '\n' || !c.is_whitespace() {
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
}

#[cfg(test)]
mod tests {
    use crate::parser::token::Symbol;

    use super::Scanner;

    #[test]
    fn scanner() {
        let tokens = Scanner::scan_tokens(
            "file.txt",
            ">>=>>=((())){{{}}}[[[]]]+++---   \r\r\r\t\t\t\n\n\n",
        );

        for tok in tokens {
            println!("{}", tok.quote())
        }
    }
}
