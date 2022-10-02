use crate::ast::arg::Arg;
use crate::ast::expr::Expr;
use crate::ast::stmt::Stmt;
use crate::compiler::compile_haystack;
use crate::error::HayError;
use crate::lex::token::{Keyword, Loc, Marker, Operator, Token, TokenKind};

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
        self.tokens.last().unwrap().kind == TokenKind::EoF
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

    fn matches(&mut self, kind: TokenKind) -> Result<Token, Token> {
        if self.check(kind) {
            Ok(self.tokens.pop().unwrap())
        } else {
            Err(self.tokens.first().unwrap().clone())
        }
    }

    fn declaration(&mut self) -> Result<Vec<Box<Stmt>>, HayError> {
        let token = self.tokens.pop().unwrap();
        match &token.kind {
            TokenKind::Keyword(Keyword::Function) => self.function(token),
            TokenKind::Keyword(Keyword::Struct) => HayError::new(
                format!("Parsing structures isn't implemented yet."),
                token.loc,
            ),
            TokenKind::Keyword(Keyword::Union) => {
                HayError::new(format!("Parsing unions isn't implemented yet."), token.loc)
            }
            TokenKind::Keyword(Keyword::Include) => self.include(token),
            TokenKind::Keyword(Keyword::Var) => HayError::new(
                format!("Parsing top level var statements isn't implemented yet."),
                token.loc,
            ),
            kind => HayError::new(
                format!("Unexpected token at top level: {:?}", kind),
                token.loc,
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
                    t.loc,
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
    fn function(&mut self, start: Token) -> Result<Vec<Box<Stmt>>, HayError> {
        let name = match self.matches(TokenKind::ident()) {
            Ok(t) => t,
            Err(t) => {
                return HayError::new(
                    format!("Expected function name, but found {:?}", t.kind),
                    t.loc,
                )
            }
        };
        if let Err(t) = self.matches(TokenKind::Marker(Marker::LeftParen)) {
            return HayError::new(
                format!(
                    "Expected `(` after function name, but found {:?} instead.",
                    t.kind
                ),
                t.loc,
            );
        }

        let inputs = self.args_list()?;

        if let Err(t) = self.matches(TokenKind::Marker(Marker::RightParen)) {
            return HayError::new(
                format!("Expected `)` after inputs, but found {:?} instead.", t.kind),
                t.loc,
            );
        }

        let outputs = match self.matches(TokenKind::Marker(Marker::Arrow)) {
            Ok(_) => {
                let tok = match self.matches(TokenKind::Marker(Marker::LeftBracket)) {
                    Ok(t) => t,
                    Err(t) => {
                        return HayError::new(
                            format!("Expected `[` after `->`, but found {:?} instead.", t.kind),
                            t.loc,
                        )
                    }
                };

                let outputs = self.unnamed_args_list()?;
                if let Err(t) = self.matches(TokenKind::Marker(Marker::RightBracket)) {
                    return HayError::new(
                        format!(
                            "Expected `]` after return types, but found {:?} instead.",
                            t.kind
                        ),
                        t.loc,
                    );
                }

                if outputs.len() == 0 {
                    return HayError::new(format!("Expected a non-empty return list."), tok.loc);
                }

                outputs
            }
            Err(_) => vec![],
        };

        let body = self.block()?;

        Ok(vec![Box::new(Stmt::Function {
            token: start,
            name,
            inputs,
            outputs,
            body,
        })])
    }

    fn args_list(&mut self) -> Result<Vec<Arg>, HayError> {
        let mut named = None;
        let mut args = vec![];
        while let Some(arg) = self.parse_arg()? {
            if named.is_none() {
                named = Some(arg.ident.is_some());
            } else if arg.ident.is_some() != named.unwrap() {
                todo!()
            }
            args.push(arg)
        }

        Ok(args)
    }

    fn unnamed_args_list(&mut self) -> Result<Vec<Arg>, HayError> {
        let args = self.args_list()?;
        if args.iter().any(|arg| arg.ident.is_some()) {
            HayError::new(
                "Not all arguments are unnamed",
                args.first().unwrap().token.loc.clone(),
            )
        } else {
            Ok(args)
        }
    }

    fn parse_type(&mut self) -> Result<Option<Token>, HayError> {
        if let Ok(star) = self.matches(TokenKind::Operator(Operator::Star)) {
            match self.parse_type()? {
                Some(typ) => {
                    let lexeme = format!("*{}", typ.lexeme);
                    Ok(Some(Token {
                        kind: TokenKind::Ident(lexeme.clone()),
                        lexeme,
                        loc: typ.loc,
                    }))
                }
                None => HayError::new(
                    format!("Expected type after {}, but found none.", Operator::Star),
                    star.loc,
                ),
            }
        } else if let Ok(ident) = self.matches(TokenKind::ident()) {
            Ok(Some(ident))
        } else {
            Ok(None)
        }
    }

    // Arg -> type_name (: IDENT)?
    fn parse_arg(&mut self) -> Result<Option<Arg>, HayError> {
        match self.parse_type()? {
            Some(token) => {
                if let Ok(_) = self.matches(TokenKind::Marker(Marker::Colon)) {
                    match self.matches(TokenKind::ident()) {
                        Ok(ident) => Ok(Some(Arg {
                            token,
                            typ: None,
                            ident: Some(ident),
                        })),
                        Err(t) => HayError::new(
                            format!(
                                "Expected an identifier after {}, but found {:?} instead.",
                                Marker::Colon,
                                t.kind
                            ),
                            t.loc,
                        ),
                    }
                } else {
                    Ok(Some(Arg {
                        token,
                        typ: None,
                        ident: None,
                    }))
                }
            }
            None => Ok(None),
        }
    }

    fn block(&mut self) -> Result<Vec<Expr>, HayError> {
        if let Err(t) = self.matches(TokenKind::Marker(Marker::LeftBrace)) {
            return HayError::new(
                format!(
                    "Expected `{{` at start of block, but found {:?} instead.",
                    t.kind
                ),
                t.loc,
            );
        }

        let mut exprs = vec![];
        while !self.is_at_end() && !self.check(TokenKind::Marker(Marker::RightBrace)) {
            exprs.push(self.expression()?);
        }

        if let Err(t) = self.matches(TokenKind::Marker(Marker::RightBrace)) {
            return HayError::new(
                format!(
                    "Expected `}}` at end of block, but found {:?} instead.",
                    t.kind
                ),
                t.loc,
            );
        }

        Ok(exprs)
    }

    fn expression(&mut self) -> Result<Expr, HayError> {
        let token = self.tokens.pop().unwrap();
        match &token.kind {
            TokenKind::Literal(_) => Ok(Expr::Literal { value: token }),
            TokenKind::Operator(_) => Ok(Expr::Operator { op: token }),
            TokenKind::Syscall(_) => Ok(Expr::Syscall { token }),
            TokenKind::Ident(_) => Ok(Expr::Ident { ident: token }),
            TokenKind::Keyword(Keyword::Cast) => self.cast(token),
            kind => HayError::new(
                format!("Not sure how to parse expression from {:?} yet", kind),
                token.loc,
            ),
        }
    }

    fn cast(&mut self, cast_tok: Token) -> Result<Expr, HayError> {
        let open = match self.matches(TokenKind::Marker(Marker::LeftParen)) {
            Ok(t) => t,
            Err(t) => {
                return HayError::new(
                    format!(
                        "Expected {} after `cast`, but found {:?}",
                        Marker::LeftParen,
                        t.kind
                    ),
                    t.loc,
                )
            }
        };

        let typ = match self.parse_type()? {
            Some(t) => t,
            None => return HayError::new("Expected type after `cast`, but found none.", open.loc),
        };

        let close = match self.matches(TokenKind::Marker(Marker::RightParen)) {
            Ok(t) => t,
            Err(t) => {
                return HayError::new(
                    format!(
                        "Expected {} after `cast`, but found {:?}",
                        Marker::RightParen,
                        t.kind
                    ),
                    t.loc,
                )
            }
        };

        let new_lexeme = format!(
            "{}{}{}{}",
            cast_tok.lexeme, open.lexeme, typ.lexeme, close.lexeme
        );
        let token = Token {
            kind: TokenKind::Keyword(Keyword::Cast),
            lexeme: new_lexeme,
            loc: Loc::new(
                cast_tok.loc.file,
                cast_tok.loc.line,
                cast_tok.loc.span.start,
                close.loc.span.end,
            ),
        };

        Ok(Expr::Cast { token, typ })
    }
}
