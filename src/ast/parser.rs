use crate::ast::expr::Expr;
use crate::ast::stmt::Stmt;
use crate::error::HayError;
use crate::lex::token::{Keyword, Loc, Marker, Operator, Token, TokenKind, TypeToken};
use crate::types::{FnTag, RecordKind};
use std::collections::HashSet;

use super::arg::{IdentArg, UntypedArg};
use super::expr::{
    ExprAccessor, ExprAnnotatedCall, ExprAs, ExprCast, ExprElseIf, ExprIdent, ExprIf, ExprLiteral,
    ExprOperator, ExprReturn, ExprSizeOf, ExprSyscall, ExprUnary, ExprVar, ExprWhile,
};
use super::member::UntypedMember;
use super::visibility::Visitiliby;

pub struct Parser<'a> {
    tokens: Vec<Token>,
    stmts: Vec<Stmt>,
    visited: &'a mut HashSet<String>,
}

impl<'a> Parser<'a> {
    pub fn new(mut tokens: Vec<Token>, visited: &'a mut HashSet<String>) -> Self {
        Parser {
            tokens: tokens.drain(..).rev().collect(),
            stmts: vec![],
            visited,
        }
    }

    pub fn parse(mut self) -> Result<Vec<Stmt>, HayError> {
        while !self.is_at_end() {
            match self.declaration() {
                Err(e) => return Err(e),
                Ok(mut stmt) => self.stmts.append(&mut stmt),
            }
        }

        Ok(self.stmts)
    }

    fn is_at_end(&self) -> bool {
        self.tokens.last().unwrap().kind == TokenKind::EoF
    }

    fn peek(&self) -> &Token {
        self.tokens.last().unwrap()
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
            Err(self.tokens.last().unwrap().clone())
        }
    }

    fn unary(&mut self, op: Token) -> Result<Box<Expr>, HayError> {
        let expr = self.expression()?;

        Ok(Box::new(Expr::Unary(ExprUnary {
            op: ExprOperator {
                op: op.operator()?,
                token: op,
            },
            expr,
        })))
    }

    fn declaration(&mut self) -> Result<Vec<Stmt>, HayError> {
        let token = self.tokens.pop().unwrap();
        match &token.kind {
            TokenKind::Keyword(Keyword::Inline) => self.inline_function(token, None),
            TokenKind::Keyword(Keyword::Function) => self.function(token, vec![], None),
            TokenKind::Keyword(Keyword::Struct) | TokenKind::Keyword(Keyword::Union) => {
                self.record(token)
            }
            TokenKind::Keyword(Keyword::Enum) => self.enumeration(token),
            TokenKind::Keyword(Keyword::Include) => self.include(token),
            TokenKind::Keyword(Keyword::Var) => {
                let expr = self.var(token.clone())?;
                Ok(vec![Stmt::Var { token, expr }])
            }
            kind => Err(HayError::new(
                format!("Unexpected top level token: {}", kind),
                token.loc,
            )),
        }
    }

    fn include(&mut self, start: Token) -> Result<Vec<Stmt>, HayError> {
        let to_include = match self.matches(TokenKind::string()) {
            Ok(t) => t.string()?,
            Err(t) => {
                return Err(HayError::new(
                    format!(
                        "Expected Str after include statement. Found {} instead.",
                        t.kind
                    ),
                    t.loc,
                ))
            }
        };
        let libs_path = format!("src/libs/{to_include}");

        if std::path::Path::new(&to_include).exists() {
            Stmt::from_file(&to_include, self.visited)
        } else if std::path::Path::new(&libs_path).exists() {
            Stmt::from_file(&libs_path, self.visited)
        } else {
            Err(HayError::new(
                format!("Failed to find file to include: {to_include}."),
                start.loc,
            ))
        }
    }

    fn inline_function(
        &mut self,
        start: Token,
        impl_on: Option<&Token>,
    ) -> Result<Vec<Stmt>, HayError> {
        let fn_tok = match self.matches(TokenKind::Keyword(Keyword::Function)) {
            Ok(tok) => tok,
            Err(tok) => {
                return Err(HayError::new(
                    format!(
                        "Expected keyword {} after {}, but found {}",
                        Keyword::Function,
                        Keyword::Include,
                        tok.kind
                    ),
                    start.loc,
                ))
            }
        };

        self.function(fn_tok, vec![FnTag::Inline], impl_on)
    }

    // Function          -> "fn" IDENT (annotations)? args_list (return_types)? block
    // annotations       -> "<" type_name ">"
    // args_list         -> "(" typed_args_list | untyped_args_list ")"
    // typed_args_list   -> (IDENT: IDENT)*
    // untyped_args_list -> IDENT*
    // return_types      -> "->" "[" IDENT+ "]"
    // block             -> "{" Expression* "}"
    fn function(
        &mut self,
        start: Token,
        mut tags: Vec<FnTag>,
        impl_on: Option<&Token>,
    ) -> Result<Vec<Stmt>, HayError> {
        let name = match self.matches(TokenKind::ident()) {
            Ok(t) => t,
            Err(t) => {
                return Err(HayError::new(
                    format!(
                        "Expected function name after {}, but found {}",
                        Keyword::Function,
                        t.kind
                    ),
                    t.loc,
                ))
            }
        };

        if name.lexeme.starts_with('+') {
            if impl_on.is_none() {
                return Err(HayError::new(
                    "On Copy functions cannot be defined outside of an impl block.",
                    name.loc,
                ));
            }

            if name.lexeme != format!("+{}", impl_on.unwrap().lexeme) {
                return Err(HayError::new("Unexpected On Copy function name.", name.loc)
                    .with_hint(format!(
                        "Expected On Copy function to be named `+{}`",
                        impl_on.unwrap().lexeme
                    )));
            }

            tags.push(FnTag::OnCopy)
        } else if name.lexeme.starts_with('-') {
            if impl_on.is_none() {
                return Err(HayError::new(
                    "On Drop functions cannot be defined outside of an impl block.",
                    name.loc,
                ));
            }

            if name.lexeme != format!("-{}", impl_on.unwrap().lexeme) {
                return Err(HayError::new("Unexpected On Drop function name.", name.loc)
                    .with_hint(format!(
                        "Expected On Drop function to be named `-{}`",
                        impl_on.unwrap().lexeme
                    )));
            }

            tags.push(FnTag::OnDrop)
        }

        let annotations = if let Ok(open) = self.matches(TokenKind::Operator(Operator::LessThan)) {
            let annotations = self.unnamed_args_list(&open)?;
            if let Err(t) = self.matches(TokenKind::Operator(Operator::GreaterThan)) {
                return Err(HayError::new(
                    format!(
                        "Expected {} after function annotations, but found {} instead.",
                        Operator::GreaterThan,
                        t.kind
                    ),
                    t.loc,
                ));
            }

            Some(annotations)
        } else {
            None
        };

        let open = match self.matches(TokenKind::Marker(Marker::LeftParen)) {
            Ok(t) => t,
            Err(t) => {
                return Err(HayError::new(
                    format!(
                        "Expected {} after function name, but found {} instead.",
                        Marker::LeftParen,
                        t.kind
                    ),
                    t.loc,
                ))
            }
        };

        let inputs = self.args_list(&open)?;

        if let Err(t) = self.matches(TokenKind::Marker(Marker::RightParen)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after inputs, but found {} instead.",
                    Marker::RightParen,
                    t.kind,
                ),
                t.loc,
            ));
        }

        let outputs = match self.matches(TokenKind::Marker(Marker::Arrow)) {
            Ok(_) => {
                let tok = match self.matches(TokenKind::Marker(Marker::LeftBracket)) {
                    Ok(t) => t,
                    Err(t) => {
                        return Err(HayError::new(
                            format!(
                                "Expected {} after {}, but found {} instead.",
                                Marker::LeftBracket,
                                Marker::Arrow,
                                t.kind
                            ),
                            t.loc,
                        ))
                    }
                };

                let outputs = self.unnamed_args_list(&tok)?;
                if let Err(t) = self.matches(TokenKind::Marker(Marker::RightBracket)) {
                    return Err(HayError::new(
                        format!(
                            "Expected {} after return types, but found {} instead.",
                            Marker::RightBracket,
                            t.kind
                        ),
                        t.loc,
                    ));
                }

                if outputs.is_empty() {
                    return Err(HayError::new("Expected a non-empty return list.", tok.loc));
                }

                outputs
            }
            Err(_) => vec![],
        };

        let body = self.block()?;

        Ok(vec![Stmt::Function {
            token: start,
            name,
            inputs,
            outputs,
            annotations,
            body,
            tags,
            impl_on: impl_on.cloned(),
        }])
    }

    fn args_list(&mut self, token: &Token) -> Result<Vec<UntypedArg>, HayError> {
        let mut args = vec![];
        while let Some(arg) = self.parse_arg()? {
            args.push(arg)
        }

        if !args.iter().all(|arg| arg.ident.is_some())
            && !args.iter().all(|arg| arg.ident.is_none())
        {
            Err(HayError::new(
                "Either all arguments or no arguments in args list must have an identifier.",
                token.loc.clone(),
            ))
        } else {
            Ok(args)
        }
    }

    fn unnamed_args_list(&mut self, token: &Token) -> Result<Vec<UntypedArg>, HayError> {
        let args = self.args_list(token)?;
        if args.iter().any(|arg| arg.ident.is_some()) {
            Err(HayError::new(
                "Not all arguments are unnamed",
                args.first().unwrap().token.loc.clone(),
            ))
        } else {
            Ok(args)
        }
    }

    fn maybe_mut_ident_list(&mut self) -> Result<Vec<IdentArg>, HayError> {
        let mut args = vec![];
        while let Some(arg) = self.maybe_mut_ident()? {
            args.push(arg)
        }

        Ok(args)
    }

    fn maybe_mut_ident(&mut self) -> Result<Option<IdentArg>, HayError> {
        let mutable = match self.matches(TokenKind::Keyword(Keyword::Mut)) {
            Ok(tok) => Some(tok),
            Err(_) => None,
        };

        match self.matches(TokenKind::ident()) {
            Ok(ident) => Ok(Some(IdentArg {
                token: ident,
                mutable,
            })),
            Err(t) => match mutable {
                Some(mut_tok) => Err(HayError::new(
                    format!(
                        "Expected an identifier after keyword {}, but found {} instead.",
                        mut_tok.kind, t.kind
                    ),
                    mut_tok.loc,
                )),
                None => Ok(None),
            },
        }
    }

    fn parse_type(&mut self) -> Result<Option<Token>, HayError> {
        if let Ok(op) = self.matches(TokenKind::Operator(Operator::Unary(Box::default()))) {
            match (op.unary_operator()?, self.parse_type()?) {
                (Operator::Ampersand, Some(typ)) => {
                    let lexeme = format!("&{}", typ.lexeme);
                    Ok(Some(Token {
                        kind: TokenKind::Type(TypeToken::Pointer {
                            inner: Box::new(typ.typ()?),
                            mutable: false,
                        }),
                        lexeme,
                        loc: typ.loc,
                    }))
                }
                (Operator::Star, Some(typ)) => {
                    let lexeme = format!("*{}", typ.lexeme);
                    Ok(Some(Token {
                        kind: TokenKind::Type(TypeToken::Pointer {
                            inner: Box::new(typ.typ()?),
                            mutable: true,
                        }),
                        lexeme,
                        loc: typ.loc,
                    }))
                }
                (op, Some(_)) => unimplemented!("Unary {op} is not supported"),
                (op, None) => unreachable!("Unary {op} with no type???"),
            }
        } else if let Ok(ident) = self.matches(TokenKind::ident()) {
            let typ = if self
                .matches(TokenKind::Operator(Operator::LessThan))
                .is_ok()
            {
                let mut inner = vec![];
                while let Some(t) = self.parse_type()? {
                    inner.push(t.typ()?);
                }

                let close = match self.matches(TokenKind::Operator(Operator::GreaterThan)) {
                    Ok(t) => t,
                    Err(t) => {
                        return Err(HayError::new(
                            format!(
                                "Expected {} after type parameters, but found {} instead.",
                                Operator::GreaterThan,
                                t.kind
                            ),
                            t.loc,
                        ))
                    }
                };

                let kind = TokenKind::Type(TypeToken::Parameterized {
                    base: ident.ident()?,
                    inner,
                });
                let lexeme = format!("{}", kind);

                Token {
                    kind,
                    lexeme,
                    loc: Loc::new(
                        ident.loc.file,
                        ident.loc.line,
                        ident.loc.span.start,
                        close.loc.span.end,
                    ),
                }
            } else {
                Token {
                    kind: TokenKind::Type(TypeToken::Base(ident.lexeme.clone())),
                    lexeme: ident.lexeme,
                    loc: ident.loc,
                }
            };

            if self.matches(TokenKind::Marker(Marker::LeftBracket)).is_ok() {
                let n = match self.matches(TokenKind::u64()) {
                    Ok(n) => n.u64()?,
                    Err(t) => {
                        return Err(HayError::new(
                            format!(
                                "Expected array size after {}, but found {}",
                                Marker::LeftBracket,
                                t.kind
                            ),
                            t.loc,
                        ))
                    }
                };

                let close = match self.matches(TokenKind::Marker(Marker::RightBracket)) {
                    Ok(t) => t,
                    Err(t) => {
                        return Err(HayError::new(
                            format!(
                                "Expected {} after array size, but found {}",
                                Marker::RightBracket,
                                t.kind
                            ),
                            t.loc,
                        ))
                    }
                };

                let kind = TokenKind::Type(TypeToken::Array {
                    base: Box::new(typ.typ()?),
                    size: n as usize,
                });
                let lexeme = format!("{}", kind);

                Ok(Some(Token {
                    kind,
                    lexeme,
                    loc: Loc::new(
                        typ.loc.file,
                        typ.loc.line,
                        typ.loc.span.start,
                        close.loc.span.end,
                    ),
                }))
            } else {
                Ok(Some(typ))
            }
        } else if let Ok(tok) = self.matches(TokenKind::Operator(Operator::Ampersand)) {
            return Err(HayError::new(
                format!(
                    "Expected type after {}, but found {} instead.",
                    Operator::Ampersand,
                    self.peek().kind
                ),
                tok.loc,
            ));
        } else if let Ok(tok) = self.matches(TokenKind::Operator(Operator::Star)) {
            return Err(HayError::new(
                format!(
                    "Expected type after {}, but found {} instead.",
                    Operator::Star,
                    self.peek().kind
                ),
                tok.loc,
            ));
        } else {
            Ok(None)
        }
    }

    // Arg -> type_name (: IDENT)?
    fn parse_arg(&mut self) -> Result<Option<UntypedArg>, HayError> {
        match self.parse_type()? {
            Some(token) => {
                if self.matches(TokenKind::Marker(Marker::Colon)).is_ok() {
                    let mutable = self.matches(TokenKind::Keyword(Keyword::Mut)).ok();

                    match self.matches(TokenKind::ident()) {
                        Ok(ident) => Ok(Some(UntypedArg {
                            token,
                            mutable,
                            ident: Some(ident),
                        })),
                        Err(t) => Err(HayError::new(
                            format!(
                                "Expected an identifier after {}, but found {} instead.",
                                Marker::Colon,
                                t.kind
                            ),
                            t.loc,
                        )),
                    }
                } else {
                    Ok(Some(UntypedArg {
                        token,
                        mutable: None,
                        ident: None,
                    }))
                }
            }
            None => Ok(None),
        }
    }

    fn block(&mut self) -> Result<Vec<Expr>, HayError> {
        if let Err(t) = self.matches(TokenKind::Marker(Marker::LeftBrace)) {
            return Err(HayError::new(
                format!(
                    "Expected {} at start of block, but found {} instead.",
                    Marker::LeftBrace,
                    t.kind
                ),
                t.loc,
            ));
        }

        let mut exprs = vec![];
        while !self.is_at_end() && !self.check(TokenKind::Marker(Marker::RightBrace)) {
            exprs.push(*self.expression()?);
        }

        if let Err(t) = self.matches(TokenKind::Marker(Marker::RightBrace)) {
            return Err(HayError::new(
                format!(
                    "Expected {} at end of block, but found {} instead.",
                    Marker::RightBrace,
                    t.kind
                ),
                t.loc,
            ));
        }

        Ok(exprs)
    }

    fn expression(&mut self) -> Result<Box<Expr>, HayError> {
        let token = self.tokens.pop().unwrap();
        match &token.kind {
            TokenKind::Literal(l) => Ok(Box::new(Expr::Literal(ExprLiteral {
                literal: l.clone(),
                token,
            }))),
            TokenKind::Syscall(n) => Ok(Box::new(Expr::Syscall(ExprSyscall { n: *n, token }))),
            TokenKind::Ident(_) => {
                let mut new_token = token.clone();
                let mut inners = vec![];
                while let Ok(dc) = self.matches(TokenKind::Marker(Marker::DoubleColon)) {
                    let next = self.tokens.pop().unwrap();
                    match &next.kind {
                        TokenKind::Operator(Operator::LessThan) => {
                            if !inners.is_empty() {
                                return Err(HayError::new(
                                    "Cannot provide annotations within this context.",
                                    next.loc,
                                ));
                            }
                            let annotations = self.unnamed_args_list(&next)?;

                            let close =
                                match self.matches(TokenKind::Operator(Operator::GreaterThan)) {
                                    Ok(t) => t,
                                    Err(t) => {
                                        return Err(HayError::new(
                                            format!(
                                        "Expected {} after call annotations, but found {} instead.",
                                        Operator::GreaterThan,
                                        t.kind
                                    ),
                                            t.loc,
                                        ))
                                    }
                                };

                            if annotations.is_empty() {
                                return Err(HayError::new(
                                    "Expected a non-zero number of annotations",
                                    next.loc,
                                ));
                            }

                            let lexeme = {
                                let mut s = format!("{}<", token.lexeme);
                                for arg in &annotations[0..annotations.len() - 1] {
                                    s += format!("{} ", arg.token.lexeme).as_str();
                                }
                                s
                            };
                            new_token = Token {
                                kind: TokenKind::Ident(lexeme.clone()),
                                lexeme,
                                loc: Loc::new(
                                    new_token.loc.file,
                                    new_token.loc.line,
                                    new_token.loc.span.start,
                                    close.loc.span.end,
                                ),
                            };

                            return Ok(Box::new(Expr::AnnotatedCall(ExprAnnotatedCall {
                                token: new_token,
                                base: token,
                                annotations,
                            })));
                        }
                        TokenKind::Ident(_) => {
                            let new_lexeme =
                                format!("{}{}{}", new_token.lexeme, dc.lexeme, next.lexeme);
                            new_token = Token {
                                kind: TokenKind::Ident(new_lexeme.clone()),
                                lexeme: new_lexeme,
                                loc: Loc::new(
                                    new_token.loc.file,
                                    new_token.loc.line,
                                    new_token.loc.span.start,
                                    next.loc.span.end,
                                ),
                            };

                            inners.push(next);
                        }
                        kind => {
                            if inners.is_empty() {
                                return Err(HayError::new(format!("Expected either an identifier or {} after {}, but found {} instead.", Operator::LessThan, Marker::DoubleColon, kind), next.loc));
                            } else {
                                return Err(HayError::new(
                                    format!(
                                        "Expected an identifier after {}, but found {} instead.",
                                        Marker::DoubleColon,
                                        kind
                                    ),
                                    next.loc,
                                ));
                            }
                        }
                    }
                }

                if inners.is_empty() {
                    Ok(Box::new(Expr::Ident(ExprIdent { ident: token })))
                } else {
                    Ok(Box::new(Expr::Accessor(ExprAccessor {
                        token: new_token,
                        ident: token,
                        inner: inners,
                    })))
                }
            }
            TokenKind::Operator(Operator::Unary(op)) => self.unary(*op.clone()),
            TokenKind::Operator(op) => Ok(Box::new(Expr::Operator(ExprOperator {
                op: op.clone(),
                token,
            }))),
            TokenKind::Keyword(Keyword::Cast) => self.cast(token),
            TokenKind::Keyword(Keyword::If) => self.if_block(token),
            TokenKind::Keyword(Keyword::As) => self.as_block(token),
            TokenKind::Keyword(Keyword::Var) => Ok(Box::new(Expr::Var(self.var(token)?))),
            TokenKind::Keyword(Keyword::While) => self.parse_while(token),
            TokenKind::Keyword(Keyword::SizeOf) => self.size_of(token),
            TokenKind::Keyword(Keyword::Return) => Ok(Box::new(Expr::Return(ExprReturn { token }))),
            kind => Err(HayError::new(
                format!("Not sure how to parse expression from {} yet", kind),
                token.loc,
            )),
        }
    }

    fn cast(&mut self, cast_tok: Token) -> Result<Box<Expr>, HayError> {
        let open = match self.matches(TokenKind::Marker(Marker::LeftParen)) {
            Ok(t) => t,
            Err(t) => {
                return Err(HayError::new(
                    format!(
                        "Expected {} after {}, but found {}",
                        Marker::LeftParen,
                        Keyword::Cast,
                        t.kind
                    ),
                    t.loc,
                ))
            }
        };

        let typ = match self.parse_type()? {
            Some(t) => t,
            None => {
                return Err(HayError::new(
                    format!(
                        "Expected type after {}, but found {} instead.",
                        Keyword::Cast,
                        self.peek().kind
                    ),
                    open.loc,
                ))
            }
        };

        let close = match self.matches(TokenKind::Marker(Marker::RightParen)) {
            Ok(t) => t,
            Err(t) => {
                return Err(HayError::new(
                    format!(
                        "Expected {} after {}, but found {}",
                        Marker::RightParen,
                        Keyword::Cast,
                        t.kind
                    ),
                    t.loc,
                ))
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

        Ok(Box::new(Expr::Cast(ExprCast { token, typ })))
    }

    // enum -> "enum" IDENT "{" IDENT+ "}"
    fn enumeration(&mut self, start: Token) -> Result<Vec<Stmt>, HayError> {
        let name = match self.matches(TokenKind::ident()) {
            Ok(t) => t,
            Err(t) => {
                return Err(HayError::new(
                    format!(
                        "Expected an identifier after {}, but found {}",
                        Keyword::Enum,
                        t.kind
                    ),
                    t.loc,
                ))
            }
        };

        if let Err(t) = self.matches(TokenKind::Marker(Marker::LeftBrace)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after enum name, but found {} instead.",
                    Marker::LeftBrace,
                    t.kind
                ),
                t.loc,
            ));
        };

        let mut variants = vec![];

        while let Ok(t) = self.matches(TokenKind::ident()) {
            variants.push(t);
        }

        if let Err(t) = self.matches(TokenKind::Marker(Marker::RightBrace)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after variants, but found {} instead.",
                    Marker::RightBrace,
                    t.kind
                ),
                t.loc,
            ));
        };

        if variants.is_empty() {
            return Err(HayError::new(
                "Enumerations must have at least one variant.",
                name.loc,
            ));
        }

        Ok(vec![Stmt::Enum {
            token: start,
            name,
            variants,
        }])
    }

    // structure -> "struct" IDENT "{" named_args_list (impls)? "}"
    // impls     -> "impl" ":" function+
    fn record(&mut self, start: Token) -> Result<Vec<Stmt>, HayError> {
        let kw = start.keyword()?;
        let kind = if kw == Keyword::Union {
            RecordKind::Union
        } else {
            RecordKind::Struct
        };

        let name = match self.matches(TokenKind::ident()) {
            Ok(t) => t,
            Err(t) => {
                return Err(HayError::new(
                    format!(
                        "Expected an identifier after {}, but found {} instead.",
                        kw, t.kind
                    ),
                    t.loc,
                ))
            }
        };

        let annotations = if let Ok(start) = self.matches(TokenKind::Operator(Operator::LessThan)) {
            let annotations = self.unnamed_args_list(&start)?;
            if let Err(t) = self.matches(TokenKind::Operator(Operator::GreaterThan)) {
                return Err(HayError::new(
                    format!(
                        "Expected {} after {} generics, but found {} instead.",
                        Operator::GreaterThan,
                        kw,
                        t.kind
                    ),
                    t.loc,
                ));
            }
            Some(annotations)
        } else {
            None
        };

        if let Ok(_) = self.matches(TokenKind::Marker(Marker::Colon)) {
            return Ok(vec![Stmt::PreDeclaration {
                token: start,
                name,
                kind,
                annotations,
            }]);
        }

        if let Err(t) = self.matches(TokenKind::Marker(Marker::LeftBrace)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after {} name, but found {} instead.",
                    Marker::LeftBrace,
                    kind,
                    t.kind
                ),
                t.loc,
            ));
        }

        let members = self.members(&start, &name, &kind)?;

        let impls = self.impl_section(&name)?;

        if let Err(t) = self.matches(TokenKind::Marker(Marker::RightBrace)) {
            return Err(HayError::new(
                format!(
                    "Expected {} to close structure, but found {} instead.",
                    Marker::RightBrace,
                    t.kind
                ),
                t.loc,
            ));
        }

        let mut stmts = vec![Stmt::Record {
            token: start,
            name,
            annotations,
            members,
            kind,
        }];

        if let Some(mut fns) = impls {
            stmts.append(&mut fns);
        }

        Ok(stmts)
    }

    fn members(
        &mut self,
        start_tok: &Token,
        typ_tok: &Token,
        kind: &RecordKind,
    ) -> Result<Vec<UntypedMember>, HayError> {
        let mut members = vec![];

        while let Some(mem) = self.member(typ_tok, kind)? {
            members.push(mem);
        }

        if members.is_empty() {
            Err(HayError::new(
                "Struct members cannot be empty.",
                start_tok.loc.clone(),
            ))
        } else {
            Ok(members)
        }
    }

    fn member(
        &mut self,
        typ: &Token,
        kind: &RecordKind,
    ) -> Result<Option<UntypedMember>, HayError> {
        let (vis, vis_tok) = match self.matches(TokenKind::Keyword(Keyword::Pub)) {
            Ok(t) => (Visitiliby::Public, Some(t)),
            Err(_) => (Visitiliby::Private, None),
        };

        let token = match (&vis, self.parse_type()?, kind) {
            (Visitiliby::Public, None, _) => {
                return Err(HayError::new(
                    format!(
                        "Expected a type after {}, but found {} instead.",
                        Keyword::Pub,
                        self.peek().kind
                    ),
                    vis_tok.unwrap().loc,
                ))
            }
            (Visitiliby::Public, _, RecordKind::Union) => {
                return Err(HayError::new(
                    "Unexpected Keyword `pub` in union definition.",
                    vis_tok.unwrap().loc,
                ))
            }
            (_, None, _) => return Ok(None),
            (_, Some(t), _) => t,
        };

        if let Err(t) = self.matches(TokenKind::Marker(Marker::Colon)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after type, but found {} instead.",
                    Marker::Colon,
                    t.kind
                ),
                t.loc,
            ));
        }

        let ident = match self.matches(TokenKind::ident()) {
            Ok(t) => t,
            Err(t) => {
                return Err(HayError::new(
                    format!("Expected an identifier, but found {} instead.", t.kind),
                    t.loc,
                ))
            }
        };

        Ok(Some(UntypedMember {
            parent: typ.clone(),
            vis: match kind {
                RecordKind::Union => Visitiliby::Public,
                RecordKind::Struct => vis,
            },
            token,
            ident,
        }))
    }

    fn impl_section(&mut self, impl_on: &Token) -> Result<Option<Vec<Stmt>>, HayError> {
        if self.matches(TokenKind::Keyword(Keyword::Impl)).is_ok() {
            if let Err(t) = self.matches(TokenKind::Marker(Marker::Colon)) {
                return Err(HayError::new(
                    format!(
                        "Expected {} after {}, but found {}",
                        Marker::Colon,
                        Keyword::Impl,
                        t.kind
                    ),
                    t.loc,
                ));
            }

            let mut fns = vec![];
            loop {
                match (
                    self.matches(TokenKind::Keyword(Keyword::Function)),
                    self.matches(TokenKind::Keyword(Keyword::Inline)),
                ) {
                    (Ok(fn_tok), _) => {
                        fns.append(&mut self.function(fn_tok, vec![], Some(impl_on))?)
                    }
                    (_, Ok(inline_tok)) => {
                        fns.append(&mut self.inline_function(inline_tok, Some(impl_on))?)
                    }
                    _ => break,
                }
            }

            Ok(Some(fns))
        } else {
            Ok(None)
        }
    }

    fn if_block(&mut self, token: Token) -> Result<Box<Expr>, HayError> {
        let then = self.block()?;
        let mut otherwise = vec![];
        let mut finally = None;
        while let Ok(else_tok) = self.matches(TokenKind::Keyword(Keyword::Else)) {
            match self.peek().kind {
                TokenKind::Marker(Marker::LeftBrace) => {
                    finally = Some(self.block()?);
                    break;
                }
                _ => {
                    let cond = self.else_if_condition()?;
                    let body = self.block()?;

                    otherwise.push(ExprElseIf {
                        token: else_tok,
                        condition: cond,
                        block: body,
                    });
                }
            }
        }

        Ok(Box::new(Expr::If(ExprIf {
            token,
            then,
            otherwise,
            finally,
        })))
    }

    fn else_if_condition(&mut self) -> Result<Vec<Expr>, HayError> {
        let mut cond = vec![];
        while self.peek().kind != TokenKind::Keyword(Keyword::If) && !self.is_at_end() {
            cond.push(*self.expression()?);
        }

        if let Err(t) = self.matches(TokenKind::Keyword(Keyword::If)) {
            return Err(HayError::new(
                format!("Expected {}, but found {} instead.", Keyword::If, t.kind),
                t.loc,
            ));
        }

        Ok(cond)
    }

    fn as_block(&mut self, token: Token) -> Result<Box<Expr>, HayError> {
        if let Err(t) = self.matches(TokenKind::Marker(Marker::LeftBracket)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after {}, but found {} instead.",
                    Marker::LeftBracket,
                    Keyword::As,
                    t.kind
                ),
                t.loc,
            ));
        }

        let idents = self.maybe_mut_ident_list()?;

        if let Err(t) = self.matches(TokenKind::Marker(Marker::RightBracket)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after {}, but found {} instead.",
                    Marker::RightBracket,
                    Keyword::As,
                    t.kind
                ),
                t.loc,
            ));
        }

        let block = if self.check(TokenKind::Marker(Marker::LeftBrace)) {
            Some(self.block()?)
        } else {
            None
        };

        Ok(Box::new(Expr::As(ExprAs {
            token,
            idents,
            block,
        })))
    }

    fn var(&mut self, token: Token) -> Result<ExprVar, HayError> {
        let typ = match self.parse_type()? {
            Some(t) => t,
            None => {
                return Err(HayError::new(
                    format!(
                        "Expected a type after {}, but found {} instead.",
                        Keyword::Var,
                        self.peek().kind
                    ),
                    token.loc,
                ))
            }
        };

        if let Err(t) = self.matches(TokenKind::Marker(Marker::Colon)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after type, but found {}",
                    Marker::Colon,
                    t.kind
                ),
                t.loc,
            ));
        }

        let ident = match self.matches(TokenKind::ident()) {
            Ok(t) => t,
            Err(t) => {
                return Err(HayError::new(
                    format!(
                        "Expected an identifier after {}, but found {} instead.",
                        TokenKind::ident(),
                        t.kind
                    ),
                    t.loc,
                ))
            }
        };

        Ok(ExprVar { token, typ, ident })
    }

    fn parse_while(&mut self, token: Token) -> Result<Box<Expr>, HayError> {
        let mut cond = vec![];
        while !self.check(TokenKind::Marker(Marker::LeftBrace)) {
            cond.push(*self.expression()?);
        }

        let body = self.block()?;

        Ok(Box::new(Expr::While(ExprWhile { token, cond, body })))
    }

    fn size_of(&mut self, token: Token) -> Result<Box<Expr>, HayError> {
        let open = match self.matches(TokenKind::Marker(Marker::LeftParen)) {
            Ok(t) => t,
            Err(t) => {
                return Err(HayError::new(
                    format!(
                        "Expected {} after {}, but found {} instead.",
                        Marker::LeftParen,
                        Keyword::SizeOf,
                        t.kind
                    ),
                    t.loc,
                ))
            }
        };

        let typ = match self.parse_type()? {
            Some(t) => t,
            None => {
                return Err(HayError::new(
                    format!(
                        "Expected a type identifier, but found {} instead.",
                        self.peek().kind
                    ),
                    open.loc,
                ))
            }
        };

        if let Err(t) = self.matches(TokenKind::Marker(Marker::RightParen)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after type identifier, but found {} instead.",
                    Marker::RightParen,
                    t.kind
                ),
                t.loc,
            ));
        }

        Ok(Box::new(Expr::SizeOf(ExprSizeOf { token, typ })))
    }
}

mod tests {

    #[test]
    fn parse_as_block_bad_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_as_block_bad_close")
    }

    #[test]
    fn parse_as_block_bad_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_as_block_bad_open")
    }

    #[test]
    fn parse_bad_accessor1() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_accessor1")
    }

    #[test]
    fn parse_bad_accessor2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_accessor2")
    }

    #[test]
    fn parse_bad_accessor3() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_accessor3")
    }

    #[test]
    fn parse_bad_annotated_call_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_annotated_call_close")
    }

    #[test]
    fn parse_bad_annotated_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_annotated_type")
    }

    #[test]
    fn parse_bad_arg_identifier() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_arg_identifier")
    }

    #[test]
    fn parse_bad_array_var_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_array_var_close")
    }

    #[test]
    fn parse_bad_array_var_size() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_array_var_size")
    }

    #[test]
    fn parse_bad_block_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_block_close")
    }

    #[test]
    fn parse_bad_block_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_block_open")
    }

    #[test]
    fn parse_bad_body_after_function_name() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_body_after_function_name")
    }

    #[test]
    fn parse_bad_cast_expr_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_cast_expr_close")
    }

    #[test]
    fn parse_bad_cast_expr_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_cast_expr_open")
    }

    #[test]
    fn parse_bad_cast_expr_param() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_cast_expr_param")
    }

    #[test]
    fn parse_bad_else_if_block() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_else_if_block")
    }

    #[test]
    fn parse_bad_expression() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_expression")
    }

    #[test]
    fn parse_bad_file_to_include() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_file_to_include")
    }

    #[test]
    fn parse_bad_function_annotation_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_function_annotation_close")
    }

    #[test]
    fn parse_bad_function_parameter_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_function_parameter_close")
    }

    #[test]
    fn parse_bad_function_return_list_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_function_return_list_close")
    }

    #[test]
    fn parse_bad_function_return_list_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_function_return_list_open")
    }

    #[test]
    fn parse_bad_include_statement() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_include_statement")
    }

    #[test]
    fn parse_bad_pointer_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_pointer_type")
    }

    #[test]
    fn parse_bad_top_level_token() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_top_level_token")
    }

    #[test]
    fn parse_enum_bad_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_enum_bad_close")
    }

    #[test]
    fn parse_enum_bad_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_enum_bad_open")
    }

    #[test]
    fn parse_enum_empty_variants() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_enum_empty_variants")
    }

    #[test]
    fn parse_enum_without_identifier() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_enum_without_identifier")
    }

    #[test]
    fn parse_function_empty_return_list() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_function_empty_return_list")
    }

    #[test]
    fn parse_function_without_name() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_function_without_name")
    }

    #[test]
    fn parse_mixed_identifier_arg_list() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_mixed_identifier_arg_list")
    }

    #[test]
    fn parse_no_args_in_annotated_call() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_no_args_in_annotated_call")
    }

    #[test]
    fn parse_struct_bad_annotations_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_struct_bad_annotations_close")
    }

    #[test]
    fn parse_struct_bad_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_struct_bad_close")
    }

    #[test]
    fn parse_struct_bad_impl_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_struct_bad_impl_open")
    }

    #[test]
    fn parse_struct_bad_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_struct_bad_open")
    }

    #[test]
    fn parse_struct_empty_members() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_struct_empty_members")
    }

    #[test]
    fn parse_struct_member_without_identifier1() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_struct_member_without_identifier1")
    }

    #[test]
    fn parse_struct_member_without_identifier2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_struct_member_without_identifier2")
    }

    #[test]
    fn parse_struct_pub_member_without_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_struct_pub_member_without_type")
    }

    #[test]
    fn parse_struct_without_identifier() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_struct_without_identifier")
    }

    #[test]
    fn parse_var_expr_bad_ident() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_var_expr_bad_ident")
    }

    #[test]
    fn parse_var_expr_bad_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_var_expr_bad_type")
    }

    #[test]
    fn parse_var_expr_missing_colon() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_var_expr_missing_colon")
    }

    #[test]
    fn parse_bad_sizeof_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_sizeof_open")
    }

    #[test]
    fn parse_bad_sizeof_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_sizeof_close")
    }

    #[test]
    fn parse_bad_follow_up_to_inline() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_follow_up_to_inline")
    }

    #[test]
    fn parse_on_copy_outside_impl() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_on_copy_outside_impl")
    }

    #[test]
    fn parse_on_drop_outside_impl() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_on_drop_outside_impl")
    }
    #[test]
    fn parse_bad_on_copy_name() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_on_copy_name")
    }
    #[test]
    fn parse_bad_on_drop_name() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_on_drop_name")
    }

    #[test]
    fn parse_bad_inner_address_of() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_inner_address_of")
    }

    #[test]
    fn parse_bad_const_ptr_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_const_ptr_type")
    }

    #[test]
    fn parse_missing_mutable_ident_in_as() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_missing_mutable_ident_in_as")
    }

    #[test]
    fn parse_mut_in_fn_output() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_mut_in_fn_output")
    }

    #[test]
    fn parse_pub_in_union() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_pub_in_union")
    }

    #[test]
    fn parse_partially_named_args() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_partially_named_args")
    }

    #[test]
    fn parse_bad_sizeof_operand() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("parser", "parse_bad_sizeof_operand")
    }
}
