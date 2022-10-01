use crate::ast::stmt::Stmt;
use crate::compiler::compile_haystack;
use crate::error::HayError;
use crate::lex::token::{Keyword, Marker, Token, TokenKind};

pub struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens.drain(..).rev().collect(),
        }
    }

    pub fn parse(mut self) -> Result<Vec<Box<Stmt>>, HayError> {
        let mut stmts = vec![];

        while !self.is_at_end() {
            match self.declaration() {
                Err(e) => return Err(e),
                Ok(mut stmt) => stmts.append(&mut stmt),
            }
        }

        Ok(stmts)
    }

    fn is_at_end(&self) -> bool {
        self.tokens.len() == 0
    }

    fn peek(&self) -> &Token {
        &self.tokens.last().unwrap()
    }

    fn check(&self, kind: TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().kind == kind
    }

    fn matches(&mut self, kind: TokenKind) -> Result<Token, &Token> {
        if self.check(kind) {
            Ok(self.tokens.pop().unwrap())
        } else {
            Err(self.tokens.first().unwrap())
        }
    }

    fn declaration(&mut self) -> Result<Vec<Box<Stmt>>, HayError> {
        let token = self.tokens.pop().unwrap();
        match &token.kind {
            TokenKind::Keyword(Keyword::Function) => self.function(token),
            TokenKind::Keyword(Keyword::Struct) => HayError::new(
                format!("Parsing structures isn't implemented yet."),
                token.loc.clone(),
            ),
            TokenKind::Keyword(Keyword::Union) => HayError::new(
                format!("Parsing unions isn't implemented yet."),
                token.loc.clone(),
            ),
            TokenKind::Keyword(Keyword::Include) => self.include(token),
            TokenKind::Keyword(Keyword::Var) => HayError::new(
                format!("Parsing top level var statements isn't implemented yet."),
                token.loc.clone(),
            ),
            kind => HayError::new(
                format!("Unexpected token at top level: {:?}", kind),
                token.loc.clone(),
            ),
        }
    }

    fn include(&mut self, start: Token) -> Result<Vec<Box<Stmt>>, HayError> {
        let to_include = match self.matches(TokenKind::string()) {
            Ok(t) => t.string()?,
            Err(t) => {
                return HayError::new(
                    format!(
                        "Expected String after include statement. Found {:?} instead",
                        t.kind
                    ),
                    t.loc.clone(),
                )
            }
        };
        let libs_path = format!("src/libs/{to_include}");

        if std::path::Path::new(&to_include).exists() {
            compile_haystack(to_include, false, false, false)
        } else if std::path::Path::new(&libs_path).exists() {
            compile_haystack(libs_path, false, false, false)
        } else {
            HayError::new(
                format!("Failed to find file to include: {to_include}."),
                start.loc,
            )
        }
    }

    // Function          -> "fn" IDENT args_list (return_types)? block
    // args_list         -> "(" typed_args_list | untyped_args_list ")"
    // typed_args_list   -> (IDENT: IDENT)*
    // untyped_args_list -> IDENT*
    // return_types      -> "->" "[" IDENT+ "]"
    // block             -> "{" expression* "}"
    fn function(&mut self, _start: Token) -> Result<Vec<Box<Stmt>>, HayError> {
        let _name_tok = match self.matches(TokenKind::ident()) {
            Ok(t) => t,
            Err(t) => {
                return HayError::new(
                    format!("Expected function name, but found {:?}", t.kind),
                    t.loc.clone(),
                )
            }
        };

        if let Err(t) = self.matches(TokenKind::Marker(Marker::LeftParen)) {
            return HayError::new(
                format!(
                    "Expected `(` after function name, but found {:?} instead.",
                    t.kind
                ),
                t.loc.clone(),
            );
        }

        todo!()
    }
}
