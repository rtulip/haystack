use std::{format, todo, unreachable};

use crate::{
    error::HayError,
    lex::token::{
        Keyword,
        Marker::{self, LeftParen},
        Operator, Token, TokenKind,
    },
    parser::{
        argument::Argument,
        expr::{ParseExpr, ParseExprKind},
        signature::Signature,
        statement::{EnumDefinition, FunctionDefinition, Statememt, StructDefinition},
        typ::{ParamatarizedIdent, ParseType, ParseTypePath},
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

    fn expect(&mut self, kind: TokenKind<'src>) -> Result<Token<'src>, HayError> {
        self.matches(kind.clone()).map_err(|token| {
            HayError::new(
                format!("Expected {}, but found {} instead", kind, token.kind),
                token.loc,
            )
        })
    }

    // [< ParseType* >]?
    fn parse_type_param_list(&mut self) -> Result<Vec<ParseType<'src>>, HayError> {
        self.strip_whitespace();
        let Ok(open) = self.matches(TokenKind::Operator(Operator::LessThan)) else {
            return Ok(vec![]);
        };

        let mut types = vec![];
        while let Some(ty) = self.parse_type()? {
            types.push(ty)
        }

        self.expect(TokenKind::Operator(Operator::GreaterThan))?;

        Ok(types)
    }

    fn parse_pointer_type(&mut self) -> Result<Option<ParseType<'src>>, HayError> {
        assert!(self
            .matches(TokenKind::Operator(Operator::Ampersand))
            .is_ok());

        let mutable = self.matches(TokenKind::Keyword(Keyword::Mut)).is_ok();

        if mutable {
            self.strip_whitespace();
        } else {
            if let Ok(token) = self.matches(TokenKind::Whitespace) {
                return Err(HayError::new(
                    format!(
                        "Expected a type after {}, but found whitespace instead",
                        Operator::Ampersand
                    ),
                    token.loc,
                ));
            }
        }

        let Some(inner) = self.parse_type()? else {
            return Err(HayError::new(
                format!(
                    "Expected type after {}, but found {} instead",
                    Operator::Ampersand,
                    self.peek().kind
                ),
                self.peek().loc.clone(),
            ));
        };

        Ok(Some(ParseType::Pointer {
            inner: Box::new(inner),
            mutable,
        }))
    }

    fn parse_type_path(&mut self) -> Result<ParseType<'src>, HayError> {
        let mut path = vec![];
        loop {
            let base = self.expect(TokenKind::ident())?;
            let params = self.parse_type_param_list()?;
            match self.matches(TokenKind::Marker(Marker::DoubleColon)) {
                Ok(dc) => path.push(ParamatarizedIdent { base, params }),
                Err(_) => {
                    if path.is_empty() {
                        return Ok(ParseType::Simple(ParamatarizedIdent { base, params }));
                    } else {
                        path.push(ParamatarizedIdent { base, params });
                        return Ok(ParseType::Path(ParseTypePath { path }));
                    }
                }
            }
        }
    }

    fn parse_tuple_type(&mut self) -> Result<ParseType<'src>, HayError> {
        let start = self.matches(TokenKind::Marker(Marker::LeftParen)).unwrap();

        let mut types = vec![];
        while let Some(ty) = self.parse_type()? {
            types.push(ty)
        }

        self.strip_whitespace();
        self.expect(TokenKind::Marker(Marker::RightParen))?;

        Ok(ParseType::Tuple(types))
    }

    fn parse_type(&mut self) -> Result<Option<ParseType<'src>>, HayError> {
        self.strip_whitespace();

        match &self.peek().kind {
            TokenKind::Ident(_) => Ok(Some(self.parse_type_path()?)),
            TokenKind::Marker(Marker::RightBrace) => Ok(None),
            TokenKind::Marker(Marker::RightParen) => Ok(None),
            TokenKind::Marker(Marker::RightBracket) => Ok(None),
            TokenKind::Marker(Marker::LeftParen) => Ok(Some(self.parse_tuple_type()?)),
            TokenKind::Marker(m) => todo!("Parse Type from {m}"),
            TokenKind::Operator(Operator::GreaterThan) => Ok(None),
            TokenKind::Operator(Operator::Ampersand) => self.parse_pointer_type(),
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
            Some(self.expect(TokenKind::ident())?)
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
        let open = self.expect(TokenKind::Marker(Marker::LeftParen))?;

        let args = self.parse_argument_list()?;

        self.strip_whitespace();
        self.expect(TokenKind::Marker(Marker::RightParen))?;

        Ok(args)
    }

    fn parse_return_list(&mut self) -> Result<Vec<ParseType<'src>>, HayError> {
        self.strip_whitespace();

        if self.matches(TokenKind::Marker(Marker::Arrow)).is_ok() {
            self.strip_whitespace();

            self.expect(TokenKind::Marker(Marker::LeftBracket))?;

            let mut types = vec![];
            while let Some(ty) = self.parse_type()? {
                types.push(ty)
            }

            self.strip_whitespace();
            self.expect(TokenKind::Marker(Marker::RightBracket))?;
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

    fn parse_block_expr(&mut self) -> Result<ParseExpr<'src>, HayError> {
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

    fn parse_pack_expr(&mut self) -> ParseExpr<'src> {
        let token = self
            .matches(TokenKind::Keyword(Keyword::Pack))
            .expect("Should have seen pack keyword before calling");

        ParseExpr {
            kind: ParseExprKind::Pack,
            token,
        }
    }

    fn parse_tuple_expr(&mut self) -> Result<ParseExpr<'src>, HayError> {
        let token = self
            .matches(TokenKind::Marker(Marker::LeftParen))
            .expect("Should have seen pack keyword before calling");

        let mut exprs = vec![];
        loop {
            self.strip_whitespace();
            if self.is_at_end() {
                todo!("Didn't find matching `)`");
            }

            if self.matches(TokenKind::Marker(Marker::RightParen)).is_ok() {
                return Ok(ParseExpr {
                    kind: ParseExprKind::Tuple(exprs),
                    token,
                });
            }

            let e = self.parse_expr()?;
            exprs.push(e)
        }
    }

    fn parse_as_expr(&mut self) -> Result<ParseExpr<'src>, HayError> {
        let token = self.matches(TokenKind::Keyword(Keyword::As)).unwrap();

        self.strip_whitespace();

        self.expect(TokenKind::Marker(Marker::LeftBracket))?;

        let mut idents = vec![];
        loop {
            self.strip_whitespace();

            if self
                .matches(TokenKind::Marker(Marker::RightBracket))
                .is_ok()
            {
                break;
            }

            let ident = self.expect(TokenKind::ident())?;
            idents.push(ident)
        }

        Ok(ParseExpr {
            kind: ParseExprKind::As(idents),
            token,
        })
    }

    fn parse_take_ref_expr(&mut self) -> Result<ParseExpr<'src>, HayError> {
        let start = self
            .matches(TokenKind::Operator(Operator::Ampersand))
            .unwrap();

        let mutable = self.matches(TokenKind::Keyword(Keyword::Mut)).is_ok();
        if mutable {
            self.strip_whitespace();
        }

        let ident = self.expect(TokenKind::ident())?;

        Ok(ParseExpr {
            kind: ParseExprKind::TakeRef {
                mutable,
                ident: ident.lexeme,
            },
            token: start,
        })
    }

    fn parse_call_expr(&mut self) -> ParseExpr<'src> {
        let token = self.matches(TokenKind::Keyword(Keyword::Call)).unwrap();
        ParseExpr {
            kind: ParseExprKind::Call,
            token,
        }
    }

    fn parse_expr(&mut self) -> Result<ParseExpr<'src>, HayError> {
        self.strip_whitespace();
        match &self.peek().kind {
            TokenKind::Ident(_) => Ok(self.parse_ident_expr()),
            TokenKind::Marker(Marker::LeftBrace) => self.parse_block_expr(),
            TokenKind::Marker(Marker::Hash) => self.parse_type_expr(),
            TokenKind::Marker(Marker::LeftParen) => self.parse_tuple_expr(),
            TokenKind::Keyword(Keyword::Pack) => Ok(self.parse_pack_expr()),
            TokenKind::Marker(m) => todo!("Parse Expr from {m}"),
            TokenKind::Operator(Operator::Ampersand) => self.parse_take_ref_expr(),
            TokenKind::Operator(_) => todo!(),
            TokenKind::Keyword(Keyword::As) => self.parse_as_expr(),
            TokenKind::Keyword(Keyword::Call) => Ok(self.parse_call_expr()),
            TokenKind::Keyword(kw) => todo!("Parse "),
            TokenKind::Literal(_) => Ok(self.parse_literal_expr()),
            TokenKind::Whitespace => todo!(),
            TokenKind::EoF => todo!(),
        }
    }

    // fn <ident> <params>? <signature> <expr>
    fn parse_function(&mut self, start: Token<'src>) -> Result<Statememt<'src>, HayError> {
        assert!(start.kind == TokenKind::Keyword(Keyword::Function));
        self.strip_whitespace();
        let name = self.expect(TokenKind::ident())?;

        let params = self.parse_type_param_list()?;
        let signature = self.parse_function_signature()?;
        let expr = self.parse_expr()?;

        Ok(Statememt::FunctionDefinition(FunctionDefinition {
            name,
            params,
            signature,
            expr,
        }))
    }

    fn parse_struct(&mut self, start: Token<'src>) -> Result<Statememt<'src>, HayError> {
        self.strip_whitespace();
        let name = self.expect(TokenKind::ident())?;

        let params = self.parse_type_param_list()?;

        self.strip_whitespace();
        let open = self
            .expect(TokenKind::Marker(Marker::LeftBrace))?;

        let mut members = vec![];
        while let Some(ty) = self.parse_type()? {
            self.strip_whitespace();
            self.expect(TokenKind::Marker(Marker::Colon))?;

            self.strip_whitespace();
            let id = self.expect(TokenKind::ident())?;

            members.push((ty, id))
        }

        self.strip_whitespace();
        self.expect(TokenKind::Marker(Marker::RightBrace))?;

        Ok(Statememt::StructDefinition(StructDefinition {
            name,
            params,
            members,
        }))
    }

    fn parse_enum(&mut self, start: Token<'src>) -> Result<Statememt<'src>, HayError> {
        self.strip_whitespace();
        let name = self.expect(TokenKind::ident())?;

        let params = self.parse_type_param_list()?;

        self.strip_whitespace();
        let open = self
            .expect(TokenKind::Marker(Marker::LeftBrace))?;

        let mut variants = vec![];
        while let Some(ty) = self.parse_type()? {
            // ty could be either an identifier or a type. If it's a type, expect a `:` followed by the name,
            // if it's an identifier, ty should be exactly ParseType::Simple(ident) and no colon is needed
            self.strip_whitespace();
            let (variant_name, ty) = match self.matches(TokenKind::Marker(Marker::Colon)) {
                Ok(_) => {
                    self.strip_whitespace();
                    let id = self.expect(TokenKind::ident())?;
                    (id, Some(ty))
                }
                Err(token) => match ty {
                    ParseType::Simple(token) => (token.base, None),
                    _ => {
                        return Err(HayError::new(
                            format!(
                                "Expected {} after {}, but found {} instead.",
                                Marker::Colon,
                                ty.token(),
                                token.kind
                            ),
                            token.loc,
                        ))
                    }
                },
            };

            variants.push((ty, variant_name))
        }

        self.strip_whitespace();
        self.expect(TokenKind::Marker(Marker::RightBrace))?;

        Ok(Statememt::EnumDefinition(EnumDefinition {
            name,
            params,
            variants,
        }))
    }

    fn parse_interface(&mut self, start: Token<'src>) -> Result<Statememt<'src>, HayError> {
        self.strip_whitespace();

        todo!()
    }

    fn parse_declaration(&mut self) -> Result<Statememt<'src>, HayError> {
        self.strip_whitespace();
        let token = self.tokens.pop().unwrap();
        match &token.kind {
            TokenKind::Keyword(Keyword::Function) => self.parse_function(token),
            TokenKind::Keyword(Keyword::Struct) => self.parse_struct(token),
            TokenKind::Keyword(Keyword::Enum) => self.parse_enum(token),
            TokenKind::Keyword(Keyword::Interface) => self.parse_interface(token),
            kind => Err(HayError::new(
                format!("Unexpected token: {kind}"),
                token.loc,
            )),
        }
    }
}
