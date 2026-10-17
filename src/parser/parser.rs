use std::unreachable;

use crate::{
    error::HayError,
    lex::token::{Keyword, Marker, Operator, Token, TokenKind},
    parser::{
        argument::Argument,
        expr::{ParseExpr, ParseExprKind},
        signature::Signature,
        statement::{FunctionDefinition, Statememt},
        typ::ParseType,
    },
};

pub struct Parser<'src> {
    tokens: Vec<Token<'src>>,
}

impl<'src> Parser<'src> {
    pub fn parse(mut tokens: Vec<Token<'src>>) -> Result<Vec<Statememt<'src>>, HayError> {
        let mut parser = Parser {
            tokens: tokens.drain(..).rev().collect(),
        };
        parser.strip_whitespace();

        let mut stmts = vec![];
        loop {
            if parser.is_at_end() {
                break;
            }
            let stmt = parser.parse_declaration()?;
            stmts.push(stmt)
        }
        Ok(stmts)
    }

    fn strip_whitespace(&mut self) {
        while let Ok(_) = self.matches(TokenKind::Whitespace) {}
    }

    fn is_at_end<'p>(&'p self) -> bool {
        self.tokens.last().unwrap().kind == TokenKind::EoF
    }

    fn peek(&self) -> &Token<'src> {
        self.tokens.last().unwrap()
    }

    fn check(&self, kind: TokenKind<'src>) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().kind == kind
        }
    }

    fn matches(&mut self, kind: TokenKind<'src>) -> Result<Token<'src>, Token<'src>> {
        if self.check(kind) {
            Ok(self.tokens.pop().unwrap())
        } else {
            Err(self.tokens.last().unwrap().clone())
        }
    }

    // [< ParseType* >]?
    fn parse_function_params(&mut self) -> Result<Vec<ParseType<'src>>, HayError> {
        self.strip_whitespace();
        let Ok(open) = self.matches(TokenKind::Operator(Operator::LessThan)) else {
            return Ok(vec![]);
        };

        let mut types = vec![];
        while let Some(ty) = self.parse_type()? {
            types.push(ty)
        }

        self.matches(TokenKind::Operator(Operator::GreaterThan))
            .map_err(|tok| {
                HayError::new(
                    format!("Expected {}, but found {}", Operator::GreaterThan, tok.kind),
                    tok.loc,
                )
            })?;

        Ok(types)
    }

    fn parse_paramatarized_type(&mut self, base: Token<'src>) -> Result<ParseType<'src>, HayError> {
        assert!(self
            .matches(TokenKind::Operator(Operator::LessThan))
            .is_ok());

        let mut params = vec![];
        while let Some(ty) = self.parse_type()? {
            params.push(ty)
        }

        self.strip_whitespace();
        self.matches(TokenKind::Operator(Operator::GreaterThan))
            .map_err(|tok| {
                HayError::new(
                    format!(
                        "Expected {}, but found {} instead",
                        Operator::GreaterThan,
                        tok.kind
                    ),
                    tok.loc,
                )
            })?;

        Ok(ParseType::Paramatarized { base, params })
    }

    fn parse_type(&mut self) -> Result<Option<ParseType<'src>>, HayError> {
        self.strip_whitespace();

        match &self.peek().kind {
            TokenKind::Ident(_) => {
                let token = self.tokens.pop().unwrap();
                match &self.peek().kind {
                    TokenKind::Operator(Operator::LessThan) => {
                        Ok(Some(self.parse_paramatarized_type(token)?))
                    }
                    _ => Ok(Some(ParseType::Simple(token))),
                }
            }
            TokenKind::Marker(Marker::RightBrace) => Ok(None),
            TokenKind::Marker(Marker::RightParen) => Ok(None),
            TokenKind::Marker(Marker::RightBracket) => Ok(None),
            TokenKind::Marker(m) => todo!("Parse Type from {m}"),
            TokenKind::Operator(Operator::GreaterThan) => Ok(None),
            TokenKind::Operator(o) => todo!("Parse type from {o}: "),
            TokenKind::Keyword(_) => todo!(),
            TokenKind::Literal(_) => todo!(),
            TokenKind::Whitespace => todo!(),
            TokenKind::EoF => todo!(),
        }
    }

    fn parse_argument(&mut self) -> Result<Option<Argument<'src>>, HayError> {
        let Some(typ) = self.parse_type()? else {
            return Ok(None);
        };
        self.strip_whitespace();
        let ident = if self.matches(TokenKind::Marker(Marker::Colon)).is_ok() {
            self.strip_whitespace();
            Some(self.matches(TokenKind::ident()).map_err(|tok| {
                HayError::new(
                    format!(
                        "Expected an identifier after {}, but found {}",
                        Marker::Colon,
                        tok.kind
                    ),
                    tok.loc,
                )
            })?)
        } else {
            None
        };

        Ok(Some(Argument { typ, ident }))
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Argument<'src>>, HayError> {
        let mut args = vec![];
        while let Some(arg) = self.parse_argument()? {
            args.push(arg);
        }

        Ok(args)
    }

    fn parse_function_arguments(&mut self) -> Result<Vec<Argument<'src>>, HayError> {
        self.strip_whitespace();
        let open = self
            .matches(TokenKind::Marker(Marker::LeftParen))
            .map_err(|tok| {
                HayError::new(
                    format!(
                        "Expected {} after function name, but found {} instead",
                        Marker::LeftParen,
                        tok.kind
                    ),
                    tok.loc,
                )
            })?;

        let args = self.parse_argument_list()?;

        self.strip_whitespace();
        self.matches(TokenKind::Marker(Marker::RightParen))
            .map_err(|tok| {
                HayError::new(
                    format!(
                        "Expected {} after function arguments, but found {} instead",
                        Marker::RightParen,
                        tok.kind
                    ),
                    tok.loc,
                )
            })?;

        Ok(args)
    }

    fn parse_return_list(&mut self) -> Result<Vec<ParseType<'src>>, HayError> {
        self.strip_whitespace();

        if self.matches(TokenKind::Marker(Marker::Arrow)).is_ok() {
            self.strip_whitespace();

            self.matches(TokenKind::Marker(Marker::LeftBracket))
                .map_err(|tok| {
                    HayError::new(
                        format!(
                            "Expected {} after {}, but found {} instead.",
                            Marker::LeftBracket,
                            Marker::Arrow,
                            tok.kind
                        ),
                        tok.loc,
                    )
                })?;

            let mut types = vec![];
            while let Some(ty) = self.parse_type()? {
                types.push(ty)
            }

            self.strip_whitespace();
            self.matches(TokenKind::Marker(Marker::RightBracket))
                .map_err(|tok| {
                    HayError::new(
                        format!(
                            "Expected {}, but found {} instead",
                            Marker::RightBracket,
                            tok.kind
                        ),
                        tok.loc,
                    )
                })?;
            Ok(types)
        } else {
            Ok(vec![])
        }
    }

    fn parse_function_signature(&mut self) -> Result<Signature<'src>, HayError> {
        let input = self.parse_function_arguments()?;
        let output = self.parse_return_list()?;
        Ok(Signature { input, output })
    }

    fn parse_block(&mut self) -> Result<ParseExpr<'src>, HayError> {
        let start = self.matches(TokenKind::Marker(Marker::LeftBrace)).unwrap();
        let mut exprs = vec![];

        self.strip_whitespace();
        while let Err(tok) = self.matches(TokenKind::Marker(Marker::RightBrace)) {
            if tok.kind == TokenKind::EoF {
                return Err(HayError::new(
                    format!(
                        "Expected {}, but found end of file instead.",
                        Marker::RightBrace
                    ),
                    tok.loc,
                )
                .with_hint(format!("Opening brace found here: {}", start.loc)));
            }
            exprs.push(self.parse_expr()?);
            self.strip_whitespace();
        }

        Ok(ParseExpr {
            kind: ParseExprKind::Block(exprs),
            token: start,
        })
    }

    fn parse_literal_expr(&mut self) -> ParseExpr<'src> {
        let token = self.tokens.pop().unwrap();
        let TokenKind::Literal(l) = &token.kind else {
            unreachable!(
                "parse_literal_expr should only be called if we know we have a literal token next"
            )
        };

        ParseExpr {
            kind: ParseExprKind::Literal(l.clone()),
            token,
        }
    }

    fn parse_ident_expr(&mut self) -> ParseExpr<'src> {
        let token = self.tokens.pop().unwrap();
        let TokenKind::Ident(i) = &token.kind else {
            unreachable!(
                "parse_literal_expr should only be called if we know we have a literal token next"
            )
        };

        ParseExpr {
            kind: ParseExprKind::Ident(*i),
            token,
        }
    }

    fn parse_type_expr(&mut self) -> Result<ParseExpr<'src>, HayError> {
        let Ok(hash) = self.matches(TokenKind::Marker(Marker::Hash)) else {
            unreachable!("Should have checked for `#` before calling")
        };

        if let Ok(tok) = self.matches(TokenKind::Whitespace) {
            return Err(HayError::new(
                format!(
                    "Expected a type after {}, but found whitespace instead.",
                    Marker::Hash
                ),
                tok.loc,
            ));
        }

        let Some(ty) = self.parse_type()? else {
            return Err(HayError::new(
                format!("Expected a type after {}", Marker::Hash),
                hash.loc,
            ));
        };

        Ok(ParseExpr {
            kind: ParseExprKind::Type(ty),
            token: hash,
        })
    }

    fn parse_expr(&mut self) -> Result<ParseExpr<'src>, HayError> {
        self.strip_whitespace();
        match &self.peek().kind {
            TokenKind::Ident(_) => Ok(self.parse_ident_expr()),
            TokenKind::Marker(Marker::LeftBrace) => self.parse_block(),
            TokenKind::Marker(Marker::Hash) => self.parse_type_expr(),
            TokenKind::Marker(m) => todo!("Parse Expr from {m}"),
            TokenKind::Operator(_) => todo!(),
            TokenKind::Keyword(_) => todo!(),
            TokenKind::Literal(_) => Ok(self.parse_literal_expr()),
            TokenKind::Whitespace => todo!(),
            TokenKind::EoF => todo!(),
        }
    }

    // fn <ident> <params>? <signature> <expr>
    fn parse_function(&mut self, start: Token<'src>) -> Result<Statememt<'src>, HayError> {
        assert!(start.kind == TokenKind::Keyword(Keyword::Function));
        self.strip_whitespace();
        let name = self.matches(TokenKind::ident()).map_err(|tok| {
            HayError::new(
                format!(
                    "Expected function name after {}, but found {}",
                    Keyword::Function,
                    tok.kind
                ),
                tok.loc,
            )
        })?;

        let params = self.parse_function_params()?;
        let signature = self.parse_function_signature()?;
        let expr = self.parse_expr()?;

        Ok(Statememt::FunctionDefinition(FunctionDefinition {
            name,
            params,
            signature,
            expr,
        }))
    }

    fn parse_declaration(&mut self) -> Result<Statememt<'src>, HayError> {
        self.strip_whitespace();
        let token = self.tokens.pop().unwrap();
        match &token.kind {
            TokenKind::Keyword(Keyword::Function) => self.parse_function(token),
            kind => Err(HayError::new(
                format!("Unexpected token: {kind}"),
                token.loc,
            )),
        }
    }
}
