use std::fmt::Debug;

use self::token::{Keyword, Literal, Symbol, Token, TokenKind};
use crate::{
    expression::{AsExpr, BlockExpr, Expr, ExprKind, IfExpr, LessThanExpr},
    parser::token::TokenShape,
    statement::FunctionStmt,
    types::{FnTy, Scheme, Ty, TyGen, TyVar},
};

pub mod quote;
pub mod scanner;
pub mod token;

pub enum ParseError<'src> {
    UnexpectedToken {
        expected: TokenShape,
        found: Token<'src>,
    },
}
impl<'src> Debug for ParseError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, found } => {
                writeln!(
                    f,
                    "{}: Parse Error -- Unexpected Token",
                    found.quote().loc()
                )?;
                write!(
                    f,
                    "  ━━ Expected `{expected:?}`, but found `{}` instead.",
                    found.quote().as_str()
                )?;
                Ok(())
            }
        }
    }
}

pub struct ParseFunction<'src> {
    name: Token<'src>,
    annotations: Vec<&'src str>,
    signature: ParseSignature<'src>,
    body: Expr<'src>,
}

impl<'src> ParseFunction<'src> {
    pub fn to_func_stmt(&self, gen: &mut TyGen) -> Result<FunctionStmt<'src>, ()> {
        Ok(FunctionStmt::new(
            self.name.clone(),
            self.body.clone(),
            self.signature.to_scheme(&self.annotations, gen)?,
        ))
    }
}

pub struct ParseTy<'src> {
    ident: Token<'src>,
}

impl<'src> ParseTy<'src> {
    fn to_concrete(&self, fresh: &[TyVar], annotations: &[&'src str]) -> Result<Ty<'src>, ()> {
        assert_eq!(fresh.len(), annotations.len());
        match self.ident.quote().as_str() {
            "u32" => Ok(Ty::U32),
            "bool" => Ok(Ty::Bool),
            "Str" => Ok(Ty::Str),
            ty if annotations.iter().find(|ann| **ann == ty).is_some() => Ok(annotations
                .iter()
                .zip(fresh.iter())
                .find(|(ann, _)| **ann == ty)
                .unwrap()
                .1
                .into()),
            _ => todo!(),
        }
    }
}

pub struct ParseSignature<'src> {
    input: Vec<ParseTy<'src>>,
    output: Vec<ParseTy<'src>>,
}

impl<'src> ParseSignature<'src> {
    fn to_scheme(&self, annotations: &[&'src str], gen: &mut TyGen) -> Result<Scheme<'src>, ()> {
        let mut input = vec![];
        let mut output = vec![];

        let free = (0..annotations.len())
            .into_iter()
            .map(|_| gen.fresh_with_var().1)
            .collect::<Vec<_>>();

        for ty in &self.input {
            input.push(ty.to_concrete(&free, annotations)?);
        }
        for ty in &self.output {
            output.push(ty.to_concrete(&free, annotations)?);
        }

        Ok(Scheme::new(free, FnTy::new(input, output)))
    }
}

pub struct Parser<'src> {
    tokens: Vec<Token<'src>>,
}

impl<'src> Parser<'src> {
    fn is_at_end(&self) -> bool {
        assert!(self.tokens.len() > 0);
        self.tokens
            .last()
            .unwrap()
            .kind()
            .is_shape(TokenShape::EndOfFile)
    }

    fn peek(&self) -> &TokenKind<'src> {
        self.tokens.last().unwrap().kind()
    }

    fn expect_exact<T: Into<TokenShape> + Copy>(
        &mut self,
        shape: T,
    ) -> Result<Token<'src>, ParseError<'src>> {
        if self.peek().is_shape(shape.into()) {
            Ok(self.tokens.pop().unwrap())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: shape.into(),
                found: self.tokens.last().unwrap().clone(),
            })
        }
    }

    fn expect<T: Into<TokenShape> + Copy>(
        &mut self,
        shape: T,
    ) -> Result<Token<'src>, ParseError<'src>> {
        let token = self.expect_exact(shape)?;
        self.discard_whitespace();
        Ok(token)
    }

    fn discard_whitespace(&mut self) {
        while self.peek().is_shape(TokenShape::Whitespace) && !self.is_at_end() {
            self.tokens.pop();
        }
    }

    fn ty(&mut self) -> Result<ParseTy<'src>, ParseError<'src>> {
        let ident = self.expect_exact(TokenShape::Identifier)?;

        self.discard_whitespace();
        Ok(ParseTy { ident })
    }

    fn arguments(&mut self) -> Result<ParseSignature<'src>, ParseError<'src>> {
        self.expect(Symbol::LeftParen)?;
        let mut input = vec![];
        let mut output = vec![];
        while let Ok(ty) = self.ty() {
            input.push(ty);
        }
        self.expect(Symbol::RightParen)?;

        if self.peek().is_shape(Symbol::Arrow) {
            self.expect(Symbol::Arrow)?;
            self.expect(Symbol::LeftBracket)?;
            while let Ok(ty) = self.ty() {
                output.push(ty);
            }
            self.expect(Symbol::RightBracket)?;
        }

        Ok(ParseSignature { input, output })
    }

    fn block(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        let open = self.expect(Symbol::LeftBrace)?;
        let mut exprs = vec![];
        while !self.peek().is_shape(Symbol::RightBrace) && !self.is_at_end() {
            exprs.push(self.expr()?);
        }
        self.expect(Symbol::RightBrace)?;

        Ok(Expr::new(BlockExpr::from(exprs), open))
    }

    fn literal(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        let lit = self.expect(TokenShape::Literal)?;
        Ok(Expr::new(lit.literal().clone(), lit))
    }

    fn identifier(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        let ident = self.expect(TokenShape::Identifier)?;
        match ident.quote().as_str() {
            "true" => Ok(Expr::new(ExprKind::Literal(true.into()), ident)),
            "false" => Ok(Expr::new(ExprKind::Literal(false.into()), ident)),
            _ => Ok(Expr::new(ExprKind::Var(ident.ident().into()), ident)),
        }
    }

    fn expr(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        match self.peek() {
            TokenKind::Symbol(Symbol::LeftBrace) => self.block(),
            lit if lit.is_shape(TokenShape::Literal) => self.literal(),
            ident if ident.is_shape(TokenShape::Identifier) => self.identifier(),
            kw_if if kw_if.is_shape(TokenShape::Keyword(Keyword::If)) => self.if_expr(),
            kw_as if kw_as.is_shape(TokenShape::Keyword(Keyword::As)) => self.as_expr(),
            lt if lt.is_shape(Symbol::LessThan) => {
                let tok = self.expect(Symbol::LessThan)?;
                Ok(Expr::new(LessThanExpr, tok))
            }
            x => todo!("{x:?}"),
        }
    }

    fn as_expr(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        let as_tok = self.expect(Keyword::As)?;
        self.expect(Symbol::LeftBracket)?;
        let mut idents = vec![];

        while !self.peek().is_shape(Symbol::RightBracket) {
            if self.is_at_end() {
                todo!()
            }

            idents.push(self.expect(TokenShape::Identifier)?.ident())
        }

        self.expect(Symbol::RightBracket)?;

        Ok(Expr::new(AsExpr::from(idents), as_tok))
    }

    fn if_expr(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        let if_tok = self.expect(Keyword::If)?;
        let then_expr = self.expr()?;
        if self.expect(Keyword::Else).is_ok() {
            let else_expr = self.expr()?;
            Ok(Expr::new(IfExpr::new_full(then_expr, else_expr), if_tok))
        } else {
            Ok(Expr::new(IfExpr::new(then_expr), if_tok))
        }
    }

    fn function_declaration(&mut self) -> Result<ParseFunction<'src>, ParseError<'src>> {
        self.expect(Keyword::Function)?;
        let name = self.expect(TokenShape::Identifier)?;

        let annotations = if self.peek().is_shape(Symbol::LessThan) {
            self.annotations()?
        } else {
            vec![]
        };

        let signature = self.arguments()?;
        let body = self.expr()?;

        Ok(ParseFunction {
            name,
            annotations,
            signature,
            body,
        })
    }

    fn annotations(&mut self) -> Result<Vec<&'src str>, ParseError<'src>> {
        self.expect(Symbol::LessThan)?;
        let mut idents = vec![];

        while !self.peek().is_shape(Symbol::GreaterThan) {
            if self.is_at_end() {
                todo!()
            }

            idents.push(self.expect(TokenShape::Identifier)?.ident())
        }

        self.expect(Symbol::GreaterThan)?;
        Ok(idents)
    }

    pub fn parse(
        tokens: Vec<Token<'src>>,
        gen: &mut TyGen,
    ) -> Result<Vec<FunctionStmt<'src>>, ParseError<'src>> {
        let mut parser = Self { tokens };

        let mut fns = vec![];
        parser.discard_whitespace();
        while !parser.peek().is_shape(TokenShape::EndOfFile) {
            fns.push(parser.function_declaration()?);
        }

        let mut stmts = vec![];
        for func in fns {
            stmts.push(func.to_func_stmt(gen).unwrap());
        }

        Ok(stmts)
    }
}
