use std::{format, todo, unreachable};

use crate::{
    error::HayError,
    lex::{
        scanner::Scanner,
        token::{
            Intrinsic, Keyword,
            Marker::{self, LeftParen},
            Operator, Token, TokenKind,
        },
    },
    parser::{
        argument::Argument,
        expr::{ParseExpr, ParseExprKind, ParseMatchExpr},
        signature::Signature,
        statement::{
            EnumDefinition, FunctionDefinition, InterfaceDefinition, InterfaceFunctionDefinition,
            InterfaceImpl, Statememt, StructDefinition,
        },
        typ::{ParamatarizedIdent, ParseType, ParseTypePath},
    },
};

use super::{
    expr::{ParseIfElseBranch, ParseIfExpr},
    statement::FunctionDeclaration,
};

pub struct Parser<'src> {
    tokens: Vec<Token<'src>>,
}

impl<'src> Parser<'src> {
    pub fn parse(mut tokens: Vec<Token<'src>>) -> Result<Vec<Statememt<'src>>, HayError> {
        let mut parser = Parser {
            tokens: tokens.drain(..).rev().collect(),
        };

        let mut stmts = vec![];
        loop {
            parser.strip_whitespace();
            if parser.is_at_end() {
                break;
            }
            stmts.extend(parser.parse_declaration()?)
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

    fn peek_n(&self, n: usize) -> Option<&Token<'src>> {
        self.tokens
            .len()
            .checked_sub(n)
            .and_then(|idx| self.tokens.get(idx))
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

    fn parse_function_type(&mut self) -> Result<Option<ParseType<'src>>, HayError> {
        let Some(tok) = self.peek_n(2) else {
            return Ok(None);
        };
        if tok.kind != TokenKind::Marker(Marker::LeftParen) {
            return Ok(None);
        }

        let token = self.expect(TokenKind::Keyword(Keyword::Function)).unwrap();
        let sig = self.parse_function_signature()?;

        Ok(Some(ParseType::Function(token, sig)))
    }

    fn parse_type(&mut self) -> Result<Option<ParseType<'src>>, HayError> {
        self.strip_whitespace();

        match &self.peek().kind {
            TokenKind::Ident(_) => Ok(Some(self.parse_type_path()?)),
            TokenKind::Marker(Marker::RightBrace) => Ok(None),
            TokenKind::Marker(Marker::RightParen) => Ok(None),
            TokenKind::Marker(Marker::RightBracket) => Ok(None),
            TokenKind::Marker(Marker::LeftParen) => Ok(Some(self.parse_tuple_type()?)),
            TokenKind::Operator(Operator::Write) => {
                Ok(Some(ParseType::Never(self.tokens.pop().unwrap())))
            }
            TokenKind::Marker(m) => todo!("Parse Type from {m}"),
            TokenKind::Operator(Operator::GreaterThan) => Ok(None),
            TokenKind::Operator(Operator::Ampersand) => self.parse_pointer_type(),
            TokenKind::Operator(o) => todo!("Parse type from {o}: "),
            TokenKind::Keyword(Keyword::Function) => self.parse_function_type(),
            TokenKind::Keyword(_) => todo!(),
            TokenKind::Intrinsic(_) => todo!(),
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

    fn parse_ident_expr(&mut self) -> Result<ParseExpr<'src>, HayError> {
        let token = self.tokens.pop().unwrap();
        let TokenKind::Ident(i) = &token.kind else {
            unreachable!(
                "parse_literal_expr should only be called if we know we have a literal token next"
            )
        };

        if self.matches(TokenKind::Marker(Marker::DoubleColon)).is_ok() {
            let ident = self.expect(TokenKind::ident())?;

            return Ok(ParseExpr {
                kind: ParseExprKind::Member {
                    base: *i,
                    member: ident.lexeme,
                },
                token,
            });
        }

        Ok(ParseExpr {
            kind: ParseExprKind::Ident(*i),
            token,
        })
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

    fn parse_intrinsic(&mut self) -> ParseExpr<'src> {
        let token = self.tokens.pop().unwrap();
        let kind = match &token.kind {
            TokenKind::Intrinsic(i) => ParseExprKind::Intrinsic(*i),
            TokenKind::Ident(_)
            | TokenKind::Marker(_)
            | TokenKind::Operator(_)
            | TokenKind::Keyword(_)
            | TokenKind::Literal(_)
            | TokenKind::Whitespace
            | TokenKind::EoF => unreachable!(),
        };

        ParseExpr { kind, token }
    }

    fn parse_expr_if_available(&mut self) -> Result<Option<ParseExpr<'src>>, HayError> {
        self.strip_whitespace();
        match &self.peek().kind {
            TokenKind::Ident(_)
            | TokenKind::Marker(Marker::LeftBrace)
            | TokenKind::Marker(Marker::Hash)
            | TokenKind::Marker(Marker::LeftParen)
            | TokenKind::Keyword(Keyword::Pack)
            | TokenKind::Keyword(Keyword::As)
            | TokenKind::Keyword(Keyword::Call)
            | TokenKind::Intrinsic(_)
            | TokenKind::Operator(Operator::Ampersand)
            | TokenKind::Literal(_) => Ok(Some(self.parse_expr()?)),
            TokenKind::Keyword(Keyword::Function) | TokenKind::Marker(Marker::RightBrace) => {
                Ok(None)
            }

            kind => todo!("Not sure if we can parse an expr from {kind}"),
        }
    }

    fn parse_if_expr(&mut self) -> Result<ParseExpr<'src>, HayError> {
        let start = self.expect(TokenKind::Keyword(Keyword::If)).unwrap();

        self.strip_whitespace();
        let then_open = self.expect(TokenKind::Marker(Marker::LeftBrace))?;
        let mut then = vec![];

        loop {
            self.strip_whitespace();
            if self.matches(TokenKind::Marker(Marker::RightBrace)).is_ok() {
                break;
            }

            then.push(self.parse_expr()?);
        }

        let mut otherwise = vec![];
        let mut finally = vec![];
        loop {
            self.strip_whitespace();
            let Ok(_) = self.matches(TokenKind::Keyword(Keyword::Else)) else {
                break;
            };

            self.strip_whitespace();
            match self.matches(TokenKind::Marker(Marker::LeftBrace)) {
                Ok(_) => {
                    let mut exprs = vec![];
                    loop {
                        self.strip_whitespace();
                        if self.matches(TokenKind::Marker(Marker::RightBrace)).is_ok() {
                            finally = exprs;
                            break;
                        }

                        if (self.is_at_end()) {
                            todo!("Error")
                        }

                        exprs.push(self.parse_expr()?);
                    }
                }
                Err(_) => {
                    let mut condition = vec![];
                    let token = loop {
                        self.strip_whitespace();
                        if let Ok(token) = self.matches(TokenKind::Keyword(Keyword::If)) {
                            break token;
                        }

                        if (self.is_at_end()) {
                            todo!("Error")
                        }

                        condition.push(self.parse_expr()?);
                    };

                    self.strip_whitespace();
                    self.expect(TokenKind::Marker(Marker::LeftBrace))?;
                    let mut body = vec![];
                    loop {
                        self.strip_whitespace();
                        if self.matches(TokenKind::Marker(Marker::RightBrace)).is_ok() {
                            break;
                        }

                        if (self.is_at_end()) {
                            todo!("Error")
                        }

                        body.push(self.parse_expr()?);
                    }

                    otherwise.push(ParseIfElseBranch {
                        token,
                        condition,
                        body,
                    })
                }
            }
        }

        Ok(ParseExpr {
            kind: ParseExprKind::If(ParseIfExpr {
                then,
                otherwise,
                finally,
            }),
            token: start,
        })
    }

    fn parse_operator_expr(&mut self) -> ParseExpr<'src> {
        let token = self.tokens.pop().unwrap();

        let kind = match &token.kind {
            TokenKind::Operator(operator) => match operator {
                Operator::Plus => ParseExprKind::Ident("add"),
                Operator::Minus => todo!(),
                Operator::Star => todo!(),
                Operator::Slash => todo!(),
                Operator::LessThan => todo!(),
                Operator::LessEqual => todo!(),
                Operator::GreaterThan => todo!(),
                Operator::GreaterEqual => todo!(),
                Operator::Equal => todo!(),
                Operator::BangEqual => todo!(),
                Operator::Modulo => todo!(),
                Operator::Read => todo!(),
                Operator::Write => todo!(),
                Operator::Ampersand => todo!(),
                Operator::Pipe => todo!(),
                Operator::Caret => todo!(),
            },
            TokenKind::Ident(_)
            | TokenKind::Marker(_)
            | TokenKind::Keyword(_)
            | TokenKind::Intrinsic(_)
            | TokenKind::Literal(_)
            | TokenKind::Whitespace
            | TokenKind::EoF => todo!(),
        };

        ParseExpr { kind, token }
    }

    fn parse_match_expr(&mut self) -> Result<ParseExpr<'src>, HayError> {
        let token = self.expect(TokenKind::Keyword(Keyword::Match)).unwrap();
        self.strip_whitespace();

        self.expect(TokenKind::Marker(Marker::LeftBrace))?;

        let mut cases = vec![];
        let mut finally = None;
        loop {
            self.strip_whitespace();

            if self.matches(TokenKind::Keyword(Keyword::Else)).is_ok() {
                finally = Some(Box::new(self.parse_expr()?));
                break;
            }

            let Some(ty) = self.parse_type()? else {
                break;
            };

            let expr = self.parse_expr()?;

            cases.push((ty, expr))
        }

        self.strip_whitespace();
        self.expect(TokenKind::Marker(Marker::RightBrace))?;

        Ok(ParseExpr {
            kind: ParseExprKind::Match(ParseMatchExpr { cases, finally }),
            token,
        })
    }

    fn parse_expr(&mut self) -> Result<ParseExpr<'src>, HayError> {
        self.strip_whitespace();
        match &self.peek().kind {
            TokenKind::Ident(_) => self.parse_ident_expr(),
            TokenKind::Marker(Marker::LeftBrace) => self.parse_block_expr(),
            TokenKind::Marker(Marker::Hash) => self.parse_type_expr(),
            TokenKind::Marker(Marker::LeftParen) => self.parse_tuple_expr(),
            TokenKind::Marker(m) => todo!("Parse Expr from {m}"),
            TokenKind::Keyword(Keyword::Pack) => Ok(self.parse_pack_expr()),
            TokenKind::Keyword(Keyword::As) => self.parse_as_expr(),
            TokenKind::Keyword(Keyword::Call) => Ok(self.parse_call_expr()),
            TokenKind::Keyword(Keyword::If) => self.parse_if_expr(),
            TokenKind::Keyword(Keyword::Match) => self.parse_match_expr(),
            TokenKind::Keyword(kw) => todo!("Parse {kw} expr"),
            TokenKind::Operator(Operator::Ampersand) => self.parse_take_ref_expr(),
            TokenKind::Operator(_) => Ok(self.parse_operator_expr()),
            TokenKind::Intrinsic(_) => Ok(self.parse_intrinsic()),
            TokenKind::Literal(_) => Ok(self.parse_literal_expr()),
            TokenKind::Whitespace => todo!(),
            TokenKind::EoF => todo!(),
        }
    }

    fn parse_function_def(
        &mut self,
        start: Token<'src>,
    ) -> Result<(Token<'src>, Vec<ParseType<'src>>, Signature<'src>), HayError> {
        assert!(start.kind == TokenKind::Keyword(Keyword::Function));
        self.strip_whitespace();
        let name = self.expect(TokenKind::ident())?;

        let params = self.parse_type_param_list()?;
        let signature = self.parse_function_signature()?;
        Ok((name, params, signature))
    }

    // fn <ident> <params>? <signature> <expr>
    fn parse_function(&mut self, start: Token<'src>) -> Result<Statememt<'src>, HayError> {
        let (name, params, signature) = self.parse_function_def(start)?;
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
        let open = self.expect(TokenKind::Marker(Marker::LeftBrace))?;

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
        let open = self.expect(TokenKind::Marker(Marker::LeftBrace))?;

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

        let name = self.expect(TokenKind::ident())?;
        let params = self.parse_type_param_list()?;
        self.strip_whitespace();
        self.expect(TokenKind::Marker(Marker::LeftBrace))?;

        let mut associated_types = vec![];
        loop {
            self.strip_whitespace();
            let Ok(_) = self.matches(TokenKind::Keyword(Keyword::Type)) else {
                break;
            };

            self.strip_whitespace();
            self.expect(TokenKind::Marker(Marker::Colon))?;
            self.strip_whitespace();
            let type_name = self.expect(TokenKind::ident())?;
            associated_types.push(type_name);
        }

        let mut funcs = vec![];
        loop {
            self.strip_whitespace();
            let Ok(start) = self.matches(TokenKind::Keyword(Keyword::Function)) else {
                break;
            };

            let (name, params, signature) = self.parse_function_def(start)?;
            let expr = self.parse_expr_if_available()?;

            funcs.push(InterfaceFunctionDefinition {
                name,
                params,
                signature,
                expr,
            })
        }

        self.expect(TokenKind::Marker(Marker::RightBrace))?;

        Ok(Statememt::InterfaceDefinition(InterfaceDefinition {
            name,
            params,
            associated_types,
            funcs,
        }))
    }

    fn parse_predicates(&mut self) -> Result<Vec<ParseType<'src>>, HayError> {
        self.strip_whitespace();
        let Ok(_) = self.matches(TokenKind::Keyword(Keyword::Requires)) else {
            return Ok(vec![]);
        };

        self.strip_whitespace();

        self.expect(TokenKind::Marker(Marker::LeftBracket))?;

        let mut predicates = vec![];
        while let Some(ty) = self.parse_type()? {
            predicates.push(ty)
        }

        self.strip_whitespace();
        self.expect(TokenKind::Marker(Marker::RightBracket))?;

        Ok(predicates)
    }

    fn parse_impl(&mut self, start: Token<'src>) -> Result<Statememt<'src>, HayError> {
        let params = self.parse_type_param_list()?;
        self.strip_whitespace();
        let name = self.expect(TokenKind::ident())?;
        self.strip_whitespace();
        let args = self.parse_type_param_list()?;
        self.strip_whitespace();
        let predicates = self.parse_predicates()?;
        self.strip_whitespace();
        self.expect(TokenKind::Marker(Marker::LeftBrace))?;

        let mut associated_types = vec![];
        loop {
            self.strip_whitespace();
            let Some(ty) = self.parse_type()? else {
                break;
            };
            self.strip_whitespace();
            self.expect(TokenKind::Marker(Marker::Colon))?;
            self.strip_whitespace();
            let type_name = self.expect(TokenKind::ident())?;

            associated_types.push((type_name, ty));
        }

        let mut impls = vec![];
        loop {
            self.strip_whitespace();
            let Ok(start) = self.matches(TokenKind::Keyword(Keyword::Function)) else {
                break;
            };

            let Statememt::FunctionDefinition(func) = self.parse_function(start)? else {
                unreachable!()
            };
            impls.push(func);
        }

        self.expect(TokenKind::Marker(Marker::RightBrace))?;

        Ok(Statememt::InterfaceImpl(InterfaceImpl {
            params,
            name,
            args,
            predicates,
            associated_types,
            impls,
        }))
    }

    fn parse_include(&mut self, start: Token<'src>) -> Result<Vec<Statememt<'src>>, HayError> {
        self.strip_whitespace();
        let path = self.expect(TokenKind::string())?;
        let file = &path.lexeme[1..path.lexeme.len() - 1];
        let source: &'src str = std::fs::read_to_string(file)
            .map_err(|e| HayError::new(format!("Failed to open file `{file}`"), path.loc.clone()))?
            .leak();

        let tokens = Scanner::new(file, &source).scan_tokens()?;

        Parser::parse(tokens)
    }

    fn parse_extern_function(&mut self, start: Token<'src>) -> Result<Statememt<'src>, HayError> {
        self.strip_whitespace();
        let fn_keyword = self.expect(TokenKind::Keyword(Keyword::Function))?;
        let (name, params, signature) = self.parse_function_def(fn_keyword)?;

        if params.len() != 0 {
            return Err(HayError::new(
                "Extern functions cannot be generic",
                params[0].token().loc,
            ));
        }

        Ok(Statememt::ExternFunctionDeclaratoin(FunctionDeclaration {
            name,
            signature,
        }))
    }

    fn parse_declaration(&mut self) -> Result<Vec<Statememt<'src>>, HayError> {
        self.strip_whitespace();
        let token = self.tokens.pop().unwrap();
        match &token.kind {
            TokenKind::Keyword(Keyword::Function) => Ok(vec![self.parse_function(token)?]),
            TokenKind::Keyword(Keyword::Struct) => Ok(vec![self.parse_struct(token)?]),
            TokenKind::Keyword(Keyword::Enum) => Ok(vec![self.parse_enum(token)?]),
            TokenKind::Keyword(Keyword::Interface) => Ok(vec![self.parse_interface(token)?]),
            TokenKind::Keyword(Keyword::Impl) => Ok(vec![self.parse_impl(token)?]),
            TokenKind::Keyword(Keyword::Include) => self.parse_include(token),
            TokenKind::Keyword(Keyword::Extern) => Ok(vec![self.parse_extern_function(token)?]),
            kind => Err(HayError::new(
                format!("Unexpected token: {kind}"),
                token.loc,
            )),
        }
    }
}
