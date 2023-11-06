use self::token::{Keyword, Literal, Symbol, Token, TokenKind};
use crate::{
    expression::{BlockExpr, Expr},
    parser::token::TokenShape,
    statement::FunctionStmt,
};

pub mod quote;
pub mod scanner;
pub mod token;

#[derive(Debug)]
pub enum ParseError<'src> {
    UnexpectedToken {
        expected: TokenShape,
        found: Token<'src>,
    },
    TODO(&'src str),
}

pub struct ParseFunction<'src> {
    name: Token<'src>,
    signature: ParseSignature<'src>,
    body: Expr<'src>,
}
pub struct ParseTy<'src> {
    ident: Token<'src>,
}

pub struct ParseSignature<'src> {
    input: Vec<ParseTy<'src>>,
    output: Vec<ParseTy<'src>>,
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
        self.expect(Symbol::LeftBrace)?;
        let mut exprs = vec![];
        while !self.peek().is_shape(Symbol::RightBrace) && !self.is_at_end() {
            exprs.push(self.expr()?);
        }
        self.expect(Symbol::RightBrace)?;

        Ok(BlockExpr::from(exprs).into())
    }

    fn literal(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        let lit = self.expect(TokenShape::Literal)?;
        Ok(lit.literal().clone().into())
    }

    fn identifier(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        let ident = self.expect(TokenShape::Identifier)?;
        Ok(Expr::Var(ident.ident().into()))
    }

    fn expr(&mut self) -> Result<Expr<'src>, ParseError<'src>> {
        match self.peek() {
            TokenKind::Symbol(Symbol::LeftBrace) => self.block(),
            lit if lit.is_shape(TokenShape::Literal) => self.literal(),
            ident if ident.is_shape(TokenShape::Identifier) => self.identifier(),
            x => todo!("{x:?}"),
        }
    }

    fn function_declaration(&mut self) -> Result<ParseFunction<'src>, ParseError<'src>> {
        self.expect(Keyword::Function)?;
        let name = self.expect(TokenShape::Identifier)?;
        let signature = self.arguments()?;
        let body = self.expr()?;

        dbg!(&body);

        Ok(ParseFunction {
            name,
            signature,
            body,
        })
    }

    pub fn parse(tokens: Vec<Token<'src>>) -> Result<Vec<ParseFunction<'src>>, ParseError<'src>> {
        let mut parser = Self { tokens };

        let mut fns = vec![];
        parser.discard_whitespace();
        while !parser.peek().is_shape(TokenShape::EndOfFile) {
            fns.push(parser.function_declaration()?);
        }

        Ok(fns)
    }
}
