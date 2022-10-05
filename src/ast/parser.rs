use crate::ast::arg::Arg;
use crate::ast::expr::Expr;
use crate::ast::stmt::{Member, Stmt};
use crate::compiler::compile_haystack;
use crate::error::HayError;
use crate::lex::token::{Keyword, Loc, Marker, Operator, Token, TokenKind, TypeToken};

use super::stmt::Visitiliby;

pub struct Parser {
    tokens: Vec<Token>,
    stmts: Vec<Stmt>,
}

impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Self {
        Parser {
            tokens: tokens.drain(..).rev().collect(),
            stmts: vec![],
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
            Err(self.tokens.last().unwrap().clone())
        }
    }

    fn declaration(&mut self) -> Result<Vec<Stmt>, HayError> {
        let token = self.tokens.pop().unwrap();
        match &token.kind {
            TokenKind::Keyword(Keyword::Function) => self.function(token),
            TokenKind::Keyword(Keyword::Struct) | TokenKind::Keyword(Keyword::Union) => {
                self.structure(token)
            }
            TokenKind::Keyword(Keyword::Enum) => self.enumeration(token),
            TokenKind::Keyword(Keyword::Include) => self.include(token),
            TokenKind::Keyword(Keyword::Var) => {
                let expr = self.var(token.clone())?;
                Ok(vec![Stmt::Var { token, expr }])
            }
            kind => HayError::new(format!("Unexpected top level token: {}", kind), token.loc),
        }
    }

    fn include(&mut self, start: Token) -> Result<Vec<Stmt>, HayError> {
        let to_include = match self.matches(TokenKind::string()) {
            Ok(t) => t.string()?,
            Err(t) => {
                return HayError::new(
                    format!(
                        "Expected Str after include statement. Found {} instead.",
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

    // Function          -> "fn" IDENT (annotations)? args_list (return_types)? block
    // annotations       -> "<" type_name ">"
    // args_list         -> "(" typed_args_list | untyped_args_list ")"
    // typed_args_list   -> (IDENT: IDENT)*
    // untyped_args_list -> IDENT*
    // return_types      -> "->" "[" IDENT+ "]"
    // block             -> "{" expression* "}"
    fn function(&mut self, start: Token) -> Result<Vec<Stmt>, HayError> {
        let name = match self.matches(TokenKind::ident()) {
            Ok(t) => t,
            Err(t) => {
                return HayError::new(
                    format!(
                        "Expected function name after {}, but found {}",
                        Keyword::Function,
                        t.kind
                    ),
                    t.loc,
                )
            }
        };

        let annotations = if let Ok(open) = self.matches(TokenKind::Operator(Operator::LessThan)) {
            let annotations = self.unnamed_args_list(&open)?;
            if let Err(t) = self.matches(TokenKind::Operator(Operator::GreaterThan)) {
                return HayError::new(
                    format!(
                        "Expected {} after function annotations, but found {} instead.",
                        Operator::GreaterThan,
                        t.kind
                    ),
                    t.loc,
                );
            }

            Some(annotations)
        } else {
            None
        };

        let open = match self.matches(TokenKind::Marker(Marker::LeftParen)) {
            Ok(t) => t,
            Err(t) => {
                return HayError::new(
                    format!(
                        "Expected {} after function name, but found {} instead.",
                        Marker::LeftParen,
                        t.kind
                    ),
                    t.loc,
                )
            }
        };

        let inputs = self.args_list(&open)?;

        if let Err(t) = self.matches(TokenKind::Marker(Marker::RightParen)) {
            return HayError::new(
                format!(
                    "Expected {} after inputs, but found {} instead.",
                    Marker::RightParen,
                    t.kind,
                ),
                t.loc,
            );
        }

        let outputs = match self.matches(TokenKind::Marker(Marker::Arrow)) {
            Ok(_) => {
                let tok = match self.matches(TokenKind::Marker(Marker::LeftBracket)) {
                    Ok(t) => t,
                    Err(t) => {
                        return HayError::new(
                            format!(
                                "Expected {} after {}, but found {} instead.",
                                Marker::LeftBracket,
                                Marker::Arrow,
                                t.kind
                            ),
                            t.loc,
                        )
                    }
                };

                let outputs = self.unnamed_args_list(&tok)?;
                if let Err(t) = self.matches(TokenKind::Marker(Marker::RightBracket)) {
                    return HayError::new(
                        format!(
                            "Expected {} after return types, but found {} instead.",
                            Marker::RightBracket,
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

        Ok(vec![Stmt::Function {
            token: start,
            name,
            inputs,
            outputs,
            annotations,
            body,
        }])
    }

    fn args_list(&mut self, token: &Token) -> Result<Vec<Arg>, HayError> {
        let mut args = vec![];
        while let Some(arg) = self.parse_arg()? {
            args.push(arg)
        }

        if !args.iter().all(|arg| arg.ident.is_some())
            && !args.iter().all(|arg| arg.ident.is_none())
        {
            HayError::new(
                format!(
                    "Either all arguments or no arguments in args list must have an identifier."
                ),
                token.loc.clone(),
            )
        } else {
            Ok(args)
        }
    }

    fn unnamed_args_list(&mut self, token: &Token) -> Result<Vec<Arg>, HayError> {
        let args = self.args_list(token)?;
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
                        kind: TokenKind::Type(TypeToken::Pointer(Box::new(typ.typ()?))),
                        lexeme,
                        loc: typ.loc,
                    }))
                }
                None => HayError::new(
                    format!(
                        "Expected type after {}, but found {} instead.",
                        Operator::Star,
                        self.peek().kind
                    ),
                    star.loc,
                ),
            }
        } else if let Ok(ident) = self.matches(TokenKind::ident()) {
            let typ = if let Ok(_) = self.matches(TokenKind::Operator(Operator::LessThan)) {
                let mut inner = vec![];
                while let Some(t) = self.parse_type()? {
                    inner.push(Box::new(t.typ()?));
                }

                let close = match self.matches(TokenKind::Operator(Operator::GreaterThan)) {
                    Ok(t) => t,
                    Err(t) => {
                        return HayError::new(
                            format!(
                                "Expected {} after type parameters, but found {} instead.",
                                Operator::GreaterThan,
                                t.kind
                            ),
                            t.loc,
                        )
                    }
                };

                let kind = TokenKind::Type(TypeToken::Parameterized {
                    base: ident.ident()?,
                    inner,
                });
                let lexeme = format!("{}", kind);

                Token {
                    kind: kind,
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

            if let Ok(_) = self.matches(TokenKind::Marker(Marker::LeftBracket)) {
                let n = match self.matches(TokenKind::u64()) {
                    Ok(n) => n.u64()?,
                    Err(t) => {
                        return HayError::new(
                            format!(
                                "Expected array size after {}, but found {}",
                                Marker::LeftBracket,
                                t.kind
                            ),
                            t.loc,
                        )
                    }
                };

                let close = match self.matches(TokenKind::Marker(Marker::RightBracket)) {
                    Ok(t) => t,
                    Err(t) => {
                        return HayError::new(
                            format!(
                                "Expected {} after array size, but found {}",
                                Marker::RightBracket,
                                t.kind
                            ),
                            t.loc,
                        )
                    }
                };

                let kind = TokenKind::Type(TypeToken::Array {
                    base: Box::new(typ.typ()?),
                    size: n,
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
                                "Expected an identifier after {}, but found {} instead.",
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

    fn block(&mut self) -> Result<Vec<Box<Expr>>, HayError> {
        if let Err(t) = self.matches(TokenKind::Marker(Marker::LeftBrace)) {
            return HayError::new(
                format!(
                    "Expected {} at start of block, but found {} instead.",
                    Marker::LeftBrace,
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
                    "Expected {} at end of block, but found {} instead.",
                    Marker::RightBrace,
                    t.kind
                ),
                t.loc,
            );
        }

        Ok(exprs)
    }

    fn expression(&mut self) -> Result<Box<Expr>, HayError> {
        let token = self.tokens.pop().unwrap();
        match &token.kind {
            TokenKind::Literal(_) => Ok(Box::new(Expr::Literal { value: token })),
            TokenKind::Operator(_) => Ok(Box::new(Expr::Operator { op: token })),
            TokenKind::Syscall(_) => Ok(Box::new(Expr::Syscall { token })),
            TokenKind::Ident(_) => {
                let mut new_token = token.clone();
                let mut inners = vec![];
                while let Ok(dc) = self.matches(TokenKind::Marker(Marker::DoubleColon)) {
                    let next = self.tokens.pop().unwrap();
                    match &next.kind {
                        TokenKind::Operator(Operator::LessThan) => {
                            if inners.len() != 0 {
                                return HayError::new(
                                    format!("Cannot provide annotations within this context."),
                                    next.loc,
                                );
                            }
                            let annotations = self.unnamed_args_list(&next)?;

                            let close =
                                match self.matches(TokenKind::Operator(Operator::GreaterThan)) {
                                    Ok(t) => t,
                                    Err(t) => {
                                        return HayError::new(
                                            format!(
                                        "Expected {} after call annotations, but found {} instead.",
                                        Operator::GreaterThan,
                                        t.kind
                                    ),
                                            t.loc,
                                        )
                                    }
                                };

                            if annotations.len() == 0 {
                                return HayError::new(
                                    "Expected a non-zero number of annotations",
                                    next.loc,
                                );
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
                                lexeme: lexeme,
                                loc: Loc::new(
                                    new_token.loc.file,
                                    new_token.loc.line,
                                    new_token.loc.span.start,
                                    close.loc.span.end,
                                ),
                            };

                            return Ok(Box::new(Expr::AnnotatedCall {
                                token: new_token,
                                base: token,
                                annotations: annotations,
                            }));
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
                                return HayError::new(format!("Expected either an identifier or {} after {}, but found {} instead.", Operator::LessThan, Marker::DoubleColon, kind), next.loc);
                            } else {
                                return HayError::new(
                                    format!(
                                        "Expected an identifier after {}, but found {} instead.",
                                        Marker::DoubleColon,
                                        kind
                                    ),
                                    next.loc,
                                );
                            }
                        }
                    }
                }

                if inners.len() == 0 {
                    Ok(Box::new(Expr::Ident { ident: token }))
                } else {
                    Ok(Box::new(Expr::Accessor {
                        token: new_token,
                        ident: token,
                        inner: inners,
                    }))
                }
            }
            TokenKind::Keyword(Keyword::Cast) => self.cast(token),
            TokenKind::Keyword(Keyword::If) => self.if_block(token),
            TokenKind::Keyword(Keyword::As) => self.as_block(token),
            TokenKind::Keyword(Keyword::Var) => self.var(token),
            TokenKind::Keyword(Keyword::While) => self.parse_while(token),
            kind => HayError::new(
                format!("Not sure how to parse expression from {} yet", kind),
                token.loc,
            ),
        }
    }

    fn cast(&mut self, cast_tok: Token) -> Result<Box<Expr>, HayError> {
        let open = match self.matches(TokenKind::Marker(Marker::LeftParen)) {
            Ok(t) => t,
            Err(t) => {
                return HayError::new(
                    format!(
                        "Expected {} after {}, but found {}",
                        Marker::LeftParen,
                        Keyword::Cast,
                        t.kind
                    ),
                    t.loc,
                )
            }
        };

        let typ = match self.parse_type()? {
            Some(t) => t,
            None => {
                return HayError::new(
                    format!(
                        "Expected type after {}, but found {} instead.",
                        Keyword::Cast,
                        self.peek().kind
                    ),
                    open.loc,
                )
            }
        };

        let close = match self.matches(TokenKind::Marker(Marker::RightParen)) {
            Ok(t) => t,
            Err(t) => {
                return HayError::new(
                    format!(
                        "Expected {} after {}, but found {}",
                        Marker::RightParen,
                        Keyword::Cast,
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

        Ok(Box::new(Expr::Cast { token, typ }))
    }

    // enum -> "enum" IDENT "{" IDENT+ "}"
    fn enumeration(&mut self, start: Token) -> Result<Vec<Stmt>, HayError> {
        let name = match self.matches(TokenKind::ident()) {
            Ok(t) => t,
            Err(t) => {
                return HayError::new(
                    format!(
                        "Expected an identifier after {}, but found {}",
                        Keyword::Enum,
                        t.kind
                    ),
                    t.loc,
                )
            }
        };

        if let Err(t) = self.matches(TokenKind::Marker(Marker::LeftBrace)) {
            return HayError::new(
                format!(
                    "Expected {} after enum name, but found {} instead.",
                    Marker::LeftBrace,
                    t.kind
                ),
                t.loc,
            );
        };

        let mut variants = vec![];

        while let Ok(t) = self.matches(TokenKind::ident()) {
            variants.push(t);
        }

        if let Err(t) = self.matches(TokenKind::Marker(Marker::RightBrace)) {
            return HayError::new(
                format!(
                    "Expected {} after variants, but found {} instead.",
                    Marker::RightBrace,
                    t.kind
                ),
                t.loc,
            );
        };

        Ok(vec![Stmt::Enum {
            token: start,
            name,
            variants,
        }])
    }

    // structure -> "struct" IDENT "{" named_args_list (impls)? "}"
    // impls     -> "impl" ":" function+
    fn structure(&mut self, start: Token) -> Result<Vec<Stmt>, HayError> {
        let kw = start.keyword()?;
        let name = match self.matches(TokenKind::ident()) {
            Ok(t) => t,
            Err(t) => {
                return HayError::new(
                    format!(
                        "Expected an identifier after {}, but found {} instead.",
                        kw, t.kind
                    ),
                    t.loc,
                )
            }
        };

        let annotations = if let Ok(start) = self.matches(TokenKind::Operator(Operator::LessThan)) {
            let annotations = self.unnamed_args_list(&start)?;
            if let Err(t) = self.matches(TokenKind::Operator(Operator::GreaterThan)) {
                return HayError::new(
                    format!(
                        "Expected {} after {} generics, but found {} instead.",
                        Operator::GreaterThan,
                        kw,
                        t.kind
                    ),
                    t.loc,
                );
            }
            Some(annotations)
        } else {
            None
        };

        if let Err(t) = self.matches(TokenKind::Marker(Marker::LeftBrace)) {
            return HayError::new(
                format!(
                    "Expected {} after structure name, but found {} instead.",
                    Marker::LeftBrace,
                    t.kind
                ),
                t.loc,
            );
        }

        let members = self.members(&start)?;

        let impls = self.impl_section()?;

        if let Err(t) = self.matches(TokenKind::Marker(Marker::RightBrace)) {
            return HayError::new(
                format!(
                    "Expected {} to close structure, but found {} instead.",
                    Marker::RightBrace,
                    t.kind
                ),
                t.loc,
            );
        }

        let mut stmts = vec![Stmt::Structure {
            token: start,
            name: name,
            annotations,
            members,
            union: if kw == Keyword::Union { true } else { false },
        }];

        if let Some(mut fns) = impls {
            stmts.append(&mut fns);
        }

        Ok(stmts)
    }

    fn members(&mut self, tok: &Token) -> Result<Vec<Member>, HayError> {
        let mut members = vec![];

        while let Some(mem) = self.member()? {
            members.push(mem);
        }

        if members.len() == 0 {
            HayError::new("Struct members cannot be empty.", tok.loc.clone())
        } else {
            Ok(members)
        }
    }

    fn member(&mut self) -> Result<Option<Member>, HayError> {
        let (vis, vis_tok) = match self.matches(TokenKind::Keyword(Keyword::Pub)) {
            Ok(t) => (Visitiliby::Public, Some(t)),
            Err(_) => (Visitiliby::Private, None),
        };

        let token = match (&vis, self.parse_type()?) {
            (Visitiliby::Public, None) => {
                return HayError::new(
                    format!(
                        "Expected a type after {}, but found {} instead.",
                        Keyword::Pub,
                        self.peek().kind
                    ),
                    vis_tok.unwrap().loc,
                )
            }
            (_, None) => return Ok(None),
            (_, Some(t)) => t,
        };

        if let Err(t) = self.matches(TokenKind::Marker(Marker::Colon)) {
            return HayError::new(
                format!(
                    "Expected {} after type, but found {} instead.",
                    Marker::Colon,
                    t.kind
                ),
                t.loc,
            );
        }

        let ident = match self.matches(TokenKind::ident()) {
            Ok(t) => t,
            Err(t) => {
                return HayError::new(
                    format!("Expected an identifier, but found {} instead.", t.kind),
                    t.loc,
                )
            }
        };

        Ok(Some(Member {
            vis,
            token,
            ident,
            typ: None,
        }))
    }

    fn impl_section(&mut self) -> Result<Option<Vec<Stmt>>, HayError> {
        if let Ok(_) = self.matches(TokenKind::Keyword(Keyword::Impl)) {
            if let Err(t) = self.matches(TokenKind::Marker(Marker::Colon)) {
                return HayError::new(
                    format!(
                        "Expected {} after {}, but found {}",
                        Marker::Colon,
                        Keyword::Impl,
                        t.kind
                    ),
                    t.loc,
                );
            }

            let mut fns = vec![];
            while let Ok(fn_tok) = self.matches(TokenKind::Keyword(Keyword::Function)) {
                fns.append(&mut self.function(fn_tok)?);
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

                    otherwise.push(Box::new(Expr::ElseIf {
                        else_tok,
                        condition: cond,
                        block: body,
                    }));
                }
            }
        }

        Ok(Box::new(Expr::If {
            token,
            then,
            otherwise,
            finally,
        }))
    }

    fn else_if_condition(&mut self) -> Result<Vec<Box<Expr>>, HayError> {
        let mut cond = vec![];
        while self.peek().kind != TokenKind::Keyword(Keyword::If) && !self.is_at_end() {
            cond.push(self.expression()?);
        }

        if let Err(t) = self.matches(TokenKind::Keyword(Keyword::If)) {
            return HayError::new(
                format!("Expected {}, but found {} instead.", Keyword::If, t.kind),
                t.loc,
            );
        }

        Ok(cond)
    }

    fn as_block(&mut self, token: Token) -> Result<Box<Expr>, HayError> {
        if let Err(t) = self.matches(TokenKind::Marker(Marker::LeftBracket)) {
            return HayError::new(
                format!(
                    "Expected {} after {}, but found {} instead.",
                    Marker::LeftBracket,
                    Keyword::As,
                    t.kind
                ),
                t.loc,
            );
        }

        let args = self.unnamed_args_list(&token)?;

        if let Err(t) = self.matches(TokenKind::Marker(Marker::RightBracket)) {
            return HayError::new(
                format!(
                    "Expected {} after {}, but found {} instead.",
                    Marker::RightBracket,
                    Keyword::As,
                    t.kind
                ),
                t.loc,
            );
        }

        let block = if self.check(TokenKind::Marker(Marker::LeftBrace)) {
            Some(self.block()?)
        } else {
            None
        };

        Ok(Box::new(Expr::As { token, args, block }))
    }

    fn var(&mut self, token: Token) -> Result<Box<Expr>, HayError> {
        let typ = match self.parse_type()? {
            Some(t) => t,
            None => {
                return HayError::new(
                    format!(
                        "Expected a type after {}, but found {} instead.",
                        Keyword::Var,
                        self.peek().kind
                    ),
                    token.loc,
                )
            }
        };

        if let Err(t) = self.matches(TokenKind::Marker(Marker::Colon)) {
            return HayError::new(
                format!(
                    "Expected {} after type, but found {}",
                    Marker::Colon,
                    t.kind
                ),
                t.loc,
            );
        }

        let ident = match self.matches(TokenKind::ident()) {
            Ok(t) => t,
            Err(t) => {
                return HayError::new(
                    format!(
                        "Expected an identifier after {}, but found {} instead.",
                        TokenKind::ident(),
                        t.kind
                    ),
                    t.loc,
                )
            }
        };

        Ok(Box::new(Expr::Var { token, typ, ident }))
    }

    fn parse_while(&mut self, token: Token) -> Result<Box<Expr>, HayError> {
        let mut cond = vec![];
        while !self.check(TokenKind::Marker(Marker::LeftBrace)) {
            cond.push(self.expression()?);
        }

        let body = self.block()?;

        Ok(Box::new(Expr::While {
            token: token,
            cond,
            body,
        }))
    }
}
