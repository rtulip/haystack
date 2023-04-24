use crate::ast::expr::{Expr, MatchCaseExpr, BlockExpr};
use crate::ast::stmt::Stmt;
use crate::error::HayError;
use crate::lex::token::{Keyword, Loc, Marker, Operator, Token, TokenKind, TypeToken, Literal};
use crate::types::{RecordKind, TypeId};
use std::collections::{HashSet};

use super::arg::{IdentArg, UntypedArg, IdentArgKind};
use super::expr::{
    AccessorExpr, AnnotatedCallExpr, AsExpr, ExprCast, ExprElseIf, ExprIdent, ExprIf, ExprLiteral,
    ExprOperator, ExprReturn, ExprSizeOf, ExprSyscall, ExprUnary, ExprVar, ExprWhile, TupleExpr, MatchExpr, MatchElseExpr, UnpackExpr,
};
use super::member::UntypedMember;
use super::stmt::{RecordStmt, EnumStmt, FunctionStmt, FunctionStubStmt, InterfaceStmt, InterfaceImplStmt, VarStmt, PreDeclarationStmt, FnTag, InterfaceId};
use super::visibility::Visibility;

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
            TokenKind::Keyword(Keyword::Struct) => self.record(token, RecordKind::Struct),
            TokenKind::Keyword(Keyword::Union) => self.record(token, RecordKind::Union),
            TokenKind::Keyword(Keyword::Enum) => self.enumeration(token),
            TokenKind::Keyword(Keyword::Include) => self.include(token),
            TokenKind::Keyword(Keyword::Var) => {
                let expr = self.var(token.clone())?;
                Ok(vec![Stmt::Var(VarStmt{ token, expr })])
            }
            TokenKind::Keyword(Keyword::Interface) => self.interface(token),
            TokenKind::Keyword(Keyword::Impl) => self.interface_impl(token),
            kind => Err(HayError::new(
                format!("Unexpected top level token: {kind}"),
                token.loc,
            )),
        }
    }

    fn requires(&mut self, start: Token) -> Result<Vec<Token>, HayError> {
        
        if let Err(t) = self.matches(TokenKind::Marker(Marker::Colon)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after keyword {}. Found {} instead.",
                    Marker::Colon,
                    Keyword::Requires,
                    t.kind
                ),
                t.loc,
            ))
        }

        if let Err(t) = self.matches(TokenKind::Marker(Marker::LeftBracket)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after {}. Found {} instead.",
                    Marker::LeftBracket,
                    Marker::Colon,
                    t.kind
                ),
                t.loc,
            ))
        }

        let mut reqs = vec![];
        while let Some(t) = self.parse_type()? {
            reqs.push(t);
        }

        if let Err(t) = self.matches(TokenKind::Marker(Marker::RightBracket)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after interface requirements. Found {} instead.",
                    Marker::RightBracket,
                    t.kind
                ),
                t.loc,
            ))
        }

        if reqs.is_empty() {
            return Err(HayError::new("Interface requirement list should not be empty.", start.loc).with_hint("Consider removing the `requires` list."))
        }

        Ok(reqs)
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
        
        if std::path::Path::new(&to_include).exists() {
            Stmt::from_file(&to_include, self.visited)
        } else {
            Err(HayError::new(
                format!("Failed to find file to include: {to_include}."),
                start.loc,
            ))
        }
    }

    fn interface_impl(&mut self, start: Token) -> Result<Vec<Stmt>, HayError> {
        let generics = match self.matches(TokenKind::Operator(Operator::LessThan)) {
            Ok(open) => {
                let annotations = self.unnamed_args_list(&open)?;
                if let Err(t) = self.matches(TokenKind::Operator(Operator::GreaterThan)) {
                    return Err(HayError::new(
                        format!(
                            "Expected {} after impl annotations, but found {} instead.",
                            Operator::GreaterThan,
                            t.kind
                        ),
                        t.loc,
                    ));
                }

                Some(annotations)
            }
            _ => None,
        };

        let interface = match self.parse_type()? {
            Some(t) => t,
            None => {
                return Err(HayError::new(
                    format!(
                        "Expected interface type after keyword {}, but found {} instead",
                        Keyword::Impl,
                        self.peek().kind
                    ),
                    start.loc,
                ))
            }
        };

        let requires = match self.matches(TokenKind::Keyword(Keyword::Requires)) {
            Ok(t) => Some(self.requires(t)?),
            Err(_) => None,
        };

        if requires.is_some() && generics.is_none(){
            return Err(HayError::new(
                "Cannot have a requires block on a non-generic interface implementation.", 
                requires.as_ref().unwrap().first().unwrap().loc.clone()
            ));
        }

        if let Err(t) = self.matches(TokenKind::Marker(Marker::LeftBrace)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after interface type, but found {} instead.",
                    Marker::LeftBrace,
                    t.kind
                ),
                t.loc,
            ));
        }

        let types = self.members( Some(&interface), false).unwrap_or(vec![]);
        let fns = self.function_list(None)?.into_iter().map(|s| match s {
            Stmt::Function(f) => f,
            _ => unreachable!(),
        }).collect();

        if let Err(t) = self.matches(TokenKind::Marker(Marker::RightBrace)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after interface implementation, but found {} instead.",
                    Marker::RightBrace,
                    t.kind
                ),
                t.loc,
            ));
        }

        Ok(vec![Stmt::InterfaceImpl(InterfaceImplStmt {
            token: start,
            interface,
            types,
            fns,
            generics, 
            requires,
        })])
    }

    fn interface(&mut self, start: Token) -> Result<Vec<Stmt>, HayError> {
        let name = match self.matches(TokenKind::ident()) {
            Ok(t) => t,
            Err(t) => {
                return Err(HayError::new(
                    format!(
                        "Expected interface name after {}, but found {}",
                        Keyword::Interface,
                        t.kind
                    ),
                    t.loc,
                ))
            }
        };

        let annotations = match self.matches(TokenKind::Operator(Operator::LessThan)) {
            Ok(open) => {
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

                annotations
            }
            Err(t) => {
                return Err(HayError::new(
                    format!(
                        "Expected {} after interface name, but found {}",
                        Operator::LessThan,
                        t.kind
                    ),
                    t.loc,
                ))
            }
        };

        let requires = match self.matches(TokenKind::Keyword(Keyword::Requires)) {
            Ok(t) => Some(self.requires(t)?),
            Err(_) => None,
        };

        if let Err(t) = self.matches(TokenKind::Marker(Marker::LeftBrace)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after interface name, but found {} instead.",
                    Marker::LeftBrace,
                    t.kind
                ),
                t.loc,
            ));
        }

        let types = self.interface_associated_types()?;

        let mut fns = vec![];
        let mut stubs = vec![];
        self.interface_functions(&name)?.into_iter().for_each(|s| match s {
            Stmt::Function(fn_stmt) => fns.push(fn_stmt),
            Stmt::FunctionStub(fn_stub) => stubs.push(fn_stub),
            _ => unreachable!(),
        });

        if let Err(t) = self.matches(TokenKind::Marker(Marker::RightBrace)) {
            return Err(HayError::new(
                format!(
                    "Expected {} after interface definition, but found {} instead.",
                    Marker::RightBrace,
                    t.kind
                ),
                t.loc,
            ));
        }

        Ok(vec![Stmt::Interface(InterfaceStmt {
            token: start,
            name,
            annotations,
            types,
            fns,
            stubs,
            requires,
        })])
    }

    fn interface_functions(&mut self, name: &Token) -> Result<Vec<Stmt>, HayError> {
        let mut fns = vec![];
        while let Ok(t) = self.matches(TokenKind::Keyword(Keyword::Function)) {
            fns.append(&mut self.function_stub_or_def(
                t,
                vec![FnTag::Interface(InterfaceId::new(&name.lexeme))],
                None,
            )?);
        }

        Ok(fns)
    }

    fn interface_associated_types(&mut self) -> Result<Vec<(TypeId, Token)>, HayError> {
        let mut types = Vec::new();
        while let Ok(id) = self.matches(TokenKind::ident()) {
            if id.ident().unwrap() != "_" {
                return Err(HayError::new(
                    format!("Expected `_` as placeholder for interface associated type, but found {} instead.", id.ident().unwrap()),
                    id.loc
                ));
            }

            if let Err(t) = self.matches(TokenKind::Marker(Marker::Colon)) {
                return Err(HayError::new(
                    format!(
                        "Expected {} after associated type placeholder, but found {} instead.",
                        Marker::Colon,
                        t.kind
                    ),
                    t.loc,
                ));
            }

            match self.matches(TokenKind::ident()) {
                Ok(t) => {
                    types.push((TypeId::new(&t.lexeme), t));
                }
                Err(t) => {
                    return Err(HayError::new(
                        format!(
                            "Expected an identifier after {}, but found {} instead",
                            Marker::Colon,
                            t.kind
                        ),
                        t.loc,
                    ))
                }
            }
        }

        Ok(types)
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

    fn function_stub(
        &mut self,
        start: Token,
        mut tags: Vec<FnTag>,
        impl_on: Option<&Token>,
    ) -> Result<
        FunctionStubStmt,
        HayError,
    > {
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

        let requires = match self.matches(TokenKind::Keyword(Keyword::Requires)) {
            Ok(t) => Some(self.requires(t)?),
            Err(_) => None,
        };

        let stub = FunctionStubStmt {
            token: start,
            name,
            inputs,
            outputs,
            annotations,
            tags,
            impl_on: impl_on.cloned(),
            requires,
        };

        Ok(stub)
    }

    fn function_stub_or_def(
        &mut self,
        start: Token,
        tags: Vec<FnTag>,
        impl_on: Option<&Token>,
    ) -> Result<Vec<Stmt>, HayError> {
        let stub =
            self.function_stub(start, tags, impl_on)?;

        if self.check(TokenKind::Marker(Marker::LeftBrace)) {
            let body = *self.expression()?;
            Ok(vec![Stmt::Function(FunctionStmt {
                token: stub.token,
                name: stub.name,
                inputs: stub.inputs,
                outputs: stub.outputs,
                annotations: stub.annotations,
                body,
                tags: stub.tags,
                impl_on: stub.impl_on,
                requires: stub.requires,
            })])
        } else {
            Ok(vec![Stmt::FunctionStub(stub)])
        }
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
        tags: Vec<FnTag>,
        impl_on: Option<&Token>,
    ) -> Result<Vec<Stmt>, HayError> {
        let stub = self.function_stub(start, tags, impl_on)?;
        let body = *self.expression()?;

        Ok(vec![Stmt::Function(FunctionStmt {
            token: stub.token,
            name: stub.name,
            inputs: stub.inputs,
            outputs: stub.outputs,
            annotations: stub.annotations,
            body,
            tags: stub.tags,
            impl_on: impl_on.cloned(),
            requires: stub.requires,
        })])
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

        if let Ok(open) = self.matches(TokenKind::Marker(Marker::LeftBracket)) {
            let ident_list = self.maybe_mut_ident_list()?;
            if ident_list.is_empty() {
                return Err(HayError::new(
                    "Tuple destructuring assignment must have at least one identifier.",
                    open.loc,
                ))
            }

            if let Err(e) = self.matches(TokenKind::Marker(Marker::RightBracket)) {
                return Err(HayError::new(
                    format!(
                        "Expected {} after identifiers, but found {} instead.", 
                        Marker::RightBracket, 
                        e.lexeme
                    ), 
                    e.loc
                ))
            }

            Ok(Some(IdentArg { kind: IdentArgKind::Tuple { args: ident_list }, mutable }))
        } else {
            match self.matches(TokenKind::ident()) {
                Ok(ident) => Ok(Some(IdentArg {
                    kind: IdentArgKind::Single { token: ident },
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
        } else if let Ok(left_bracket) =  self.matches(TokenKind::Marker(Marker::LeftBracket)) {
            
            let mut inner = vec![];

            while let Some(typ) = self.parse_type()? {
                inner.push(typ);
            }

            let new_tok = match self.matches(TokenKind::Marker(Marker::RightBracket)) {
                Ok(x) => {
                    let mut lexeme = left_bracket.lexeme;

                    for t in &inner {
                        lexeme = format!("{lexeme} {t}");
                    }
                    
                    lexeme = format!("{lexeme}]");
                    Token::new(TokenKind::Type(
                        TypeToken::Tuple { 
                            inner,
                            idents: None,
                        }
                    ), 
                    lexeme, 
                    left_bracket.loc.file, 
                    left_bracket.loc.line, 
                    left_bracket.loc.span.start, 
                    x.loc.span.end)
                },
                Err(e) => return Err(HayError::new(
                    format!(
                        "Expected a {} to close the tuple, but found {} instead", 
                        Marker::RightBracket,
                         e.lexeme
                    ), 
                    e.loc,
                )),
            };

            Ok(Some(new_tok))

        } else if let Ok(left_bracket) =  self.matches(TokenKind::Marker(Marker::LeftBrace)) {
            
            let members = self.members(None, false)?;

            let mut inner = vec![];
            let mut idents = vec![];

            members.into_iter().for_each(|m| {
                inner.push(m.token);
                idents.push(m.ident);
            });

            let new_tok = match self.matches(TokenKind::Marker(Marker::RightBrace)) {
                Ok(x) => {
                    let mut lexeme = left_bracket.lexeme;

                    for (t, id) in inner.iter().zip(idents.iter()) {
                        lexeme = format!("{lexeme} {}: {}", t.lexeme, id.lexeme);
                    }
                    
                    lexeme = format!("{lexeme}]");
                    Token::new(TokenKind::Type(
                        TypeToken::Tuple { 
                            inner,
                            idents: Some(idents)
                        }
                    ), 
                    lexeme, 
                    left_bracket.loc.file, 
                    left_bracket.loc.line, 
                    left_bracket.loc.span.start, 
                    x.loc.span.end)
                },
                Err(e) => return Err(HayError::new(
                    format!(
                        "Expected a {} to close the tuple, but found {} instead", 
                        Marker::RightBrace,
                         e.lexeme
                    ), 
                    e.loc,
                )),
            };

            Ok(Some(new_tok))

        } else if let Ok(ident) = self.matches(TokenKind::ident()) {
            let mut typ_tok = if self
                .matches(TokenKind::Operator(Operator::LessThan))
                .is_ok()
            {
                let mut inner = vec![];
                while let Some(t) = self.parse_type()? {
                    inner.push(t.typ()?);
                }

                if let Err(t) = self.matches(TokenKind::Operator(Operator::GreaterThan)) {    
                        match &t.kind {
                            TokenKind::Operator(Operator::ShiftRight) => {
                                self.tokens.pop();
                                let (_, second) = t.split_op()?;
                                self.tokens.push(second);
                            },
                            _ => return Err(HayError::new(
                                format!(
                                    "Expected {} after type parameters, but found {} instead.",
                                    Operator::GreaterThan,
                                    t.kind
                                ),
                                t.loc,
                            ))
                        }
                };
                TypeToken::Parameterized { base: ident.ident()?, inner }
            } else {
                TypeToken::Base(ident.lexeme.clone())
            };

            if self.matches(TokenKind::Marker(Marker::DoubleColon)).is_ok() {
                match self.matches(TokenKind::ident()) {
                    Ok(t) => {
                        typ_tok = TypeToken::Associated { base: Box::new(typ_tok), typ: t.ident()? };
                    },
                    Err(t) => return Err(HayError::new(
                        format!(
                            "Expected an identifier after {}, but found {} instead.",
                            Marker::DoubleColon,
                            t.kind
                        ),
                        t.loc,
                    ))
                }
            }

            let kind = TokenKind::Type(typ_tok);

            let tok = Token {
                lexeme: format!("{kind}"),
                kind,
                loc: ident.loc
            };

            Ok(Some(tok))
            
        } else if let Ok(tok) = self.matches(TokenKind::Operator(Operator::Write)) {
            
            Ok(Some(Token {
                lexeme: String::from("!"),
                kind: TokenKind::Type(TypeToken::Base(String::from("!"))),
                loc: tok.loc
            }))

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

    fn block(&mut self, open: Token) -> Result<Box<Expr>, HayError> {
        if !matches!(open.kind, TokenKind::Marker(Marker::LeftBrace)) {
            return Err(HayError::new(
                format!("Expected {} to open a block, but found {} instead", Marker::LeftBrace, open.kind), 
                open.loc
            ));
        }
        
        let mut exprs = vec![];
        while !self.is_at_end() && !self.check(TokenKind::Marker(Marker::RightBrace)) {
            exprs.push(*self.expression()?);
        }

        let close = match self.matches(TokenKind::Marker(Marker::RightBrace)) {
            Ok(close) => close,
            Err(t) => return Err(HayError::new(
                format!(
                    "Expected {} at end of block, but found {} instead.",
                    Marker::RightBrace,
                    t.kind
                ),
                t.loc,
            )),
        };

        Ok(Box::new(Expr::Block(BlockExpr{ open, exprs, close })))
    }

    fn expression(&mut self) -> Result<Box<Expr>, HayError> {
        let token = self.tokens.pop().unwrap();
        match &token.kind {
            TokenKind::Marker(Marker::LeftBrace) => self.block(token),
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
                                s += format!("{}>", annotations.last().unwrap().token.lexeme).as_str();
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

                            return Ok(Box::new(Expr::AnnotatedCall(AnnotatedCallExpr {
                                token: new_token,
                                base: token,
                                annotations,
                            })));
                        }
                        TokenKind::Ident(_) | TokenKind::Literal(Literal::U64(_)) => {
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
                                return Err(HayError::new(
                                    format!("Expected either an identifier or {} after {}, but found {} instead.",
                                        Operator::LessThan, 
                                        Marker::DoubleColon, 
                                        kind
                                    ), 
                                    next.loc
                                ));
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
                    Ok(Box::new(Expr::Accessor(AccessorExpr {
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
            TokenKind::Keyword(Keyword::Match) => self.match_expr(token),
            TokenKind::Keyword(Keyword::Unpack) => Ok(Box::new(Expr::Unpack(UnpackExpr { token}))),
            TokenKind::Marker(Marker::LeftBracket) => self.parse_tuple_expr(token),
            kind => Err(HayError::new(
                format!("Not sure how to parse expression from {kind} yet"),
                token.loc,
            )),
        }
    }

    fn match_expr(&mut self, token: Token) -> Result<Box<Expr>, HayError> {

        if let Err(e) = self.matches(TokenKind::Marker(Marker::LeftBrace)) {
            return Err(HayError::new(format!("Expected {} after {}, but found {} instead", Marker::LeftBrace, Keyword::Match, &e.kind), e.loc))
        }

        let mut cases = vec![];

        while let Some(variant) = self.parse_type()? {            
            let ident = match self.matches(TokenKind::Keyword(Keyword::As)) {
                Ok(as_kw) => {
                    if let Err(e) = self.matches(TokenKind::Marker(Marker::LeftBracket)) {
                        return Err(HayError::new(
                            format!(
                                "Expected {} after {}, but found {} instead", 
                                Marker::LeftBracket, 
                                Keyword::As, 
                                e.kind
                            ), 
                            e.loc
                        ))
                    }
                    
                    let mut idents = self.maybe_mut_ident_list()?;
                    if let Err(e) = self.matches(TokenKind::Marker(Marker::RightBracket)) {
                        return Err(HayError::new(
                            format!(
                                "Expected {} after {} case assignments, but found {} instead", 
                                Marker::RightBracket,
                                Keyword::Match, 
                                e.kind
                            ), 
                            e.loc
                        ))
                    }
    
                    if idents.len() != 1 {
                        return Err(
                            HayError::new(
                                format!(
                                    "{} case statement as block expects only one ident.", 
                                    Keyword::Match
                                ), 
                                as_kw.loc
                            ).with_hint(
                                format!(
                                    "Found: {:?}", 
                                    idents.iter()
                                        .map(|arg| arg.kind.to_string())
                                        .collect::<Vec<_>>()
                                )
                            )
                        )
                    }
                    Some(idents.pop().unwrap())
                },
                Err(_) => None,
            };

            // if let Err(e) = self.matches(TokenKind::Marker(Marker::LeftBrace)) {
            //     return Err(HayError::new(
            //         format!(
            //             "Expected {} to open match case expression, but found {} instead", 
            //             Marker::LeftBrace, 
            //             e.kind
            //         ), 
            //         e.loc
            //     ))
            // }

            let body = *self.expression()?;

            // if let Err(e) = self.matches(TokenKind::Marker(Marker::RightBrace)) {
            //     return Err(HayError::new(
            //         format!(
            //             "Expected {} after match case block, but found {} instead", 
            //             Marker::RightBrace, 
            //             e.kind
            //         ), 
            //         e.loc
            //     ))
            // }

            cases.push(MatchCaseExpr {
                variant, 
                ident,
                body
            })
        } 


        let finally = match self.matches(TokenKind::Keyword(Keyword::Else)) {
            Ok(else_kw) => {
                let open = match self.matches(TokenKind::Marker(Marker::LeftBrace)) {
                    Ok(open) => open,
                    Err(t) => return Err(HayError::new(
                        format!("Expected {} after {}, but found {} instead.", Marker::LeftBrace, Keyword::Else, t.kind),
                        self.peek().loc.clone()))
                };

                Some(MatchElseExpr {
                    token: else_kw,
                    body: self.block(open)?
                })
            },
            Err(_) => None
        };

        if let Err(e) = self.matches(TokenKind::Marker(Marker::RightBrace)) {
            return Err(HayError::new(format!("Expected {} after {} cases, but found {} instead", Marker::RightBrace, Keyword::Match, &e.kind), e.loc))
        }

        Ok(Box::new(Expr::Match(MatchExpr{ token, cases, else_case: finally})))
    }

    fn parse_tuple_expr(&mut self, token: Token) -> Result<Box<Expr>, HayError> {

        let mut exprs = vec![];
        loop {
            match self.matches(TokenKind::Marker(Marker::RightBracket)) {
                Ok(_) => {
                    break;
                }
                Err(e) => {
                    if self.is_at_end() {
                        return Err(HayError::new(format!("Expected {}, but found end of file instead.", Marker::RightBracket), e.loc))
                    }
                    exprs.push(*self.expression()?);
                }
            }            
        }
        
        let tuple_expr = Expr::Tuple(
            TupleExpr {
                token,
                exprs,
            }
        );

        Ok(Box::new(tuple_expr))
        
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

    fn enum_struct(&mut self, struct_token: Token) -> Result<Vec<Stmt>, HayError> {
        self.record(struct_token, RecordKind::EnumStruct)
    }

    // enum -> "enum" IDENT "{" IDENT+ "}"
    fn enumeration(&mut self, start: Token) -> Result<Vec<Stmt>, HayError> {
        if let Ok(struct_token) = self.matches(TokenKind::Keyword(Keyword::Struct)) {
            return self.enum_struct(struct_token);
        }

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

        Ok(vec![Stmt::Enum(EnumStmt {
            token: start,
            name,
            variants,
        })])
    }

    // structure -> "struct" IDENT "{" named_args_list (impls)? "}"
    // impls     -> "impl" ":" function+
    fn record(&mut self, start: Token, kind: RecordKind) -> Result<Vec<Stmt>, HayError> {
        let kw = start.keyword()?;
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

        if self.matches(TokenKind::Marker(Marker::Colon)).is_ok() {
            return Ok(vec![Stmt::PreDeclaration(PreDeclarationStmt {
                token: start,
                name,
                kind,
                annotations,
            })]);
        }

        let requires = match self.matches(TokenKind::Keyword(Keyword::Requires)) {
            Ok(t) => Some(self.requires(t)?),
            Err(_) => None,
        };

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

        let members = self.members(Some(&name), match kind {
            RecordKind::Struct => true,
            _ => false
        })?;

        if members.is_empty() {
            return Err(HayError::new(format!("{kind} members cannot be empty."), name.loc));
        }

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

        let mut stmts = vec![Stmt::Record(RecordStmt {
            token: start,
            name,
            annotations,
            members,
            kind,
            requires,
        })];

        if let Some(mut fns) = impls {
            stmts.append(&mut fns);
        }

        Ok(stmts)
    }

    fn members(
        &mut self,
        typ_tok: Option<&Token>,
        allow_pub: bool
    ) -> Result<Vec<UntypedMember>, HayError> {
        let mut members = vec![];

        while let Some(mem) = self.member(typ_tok, allow_pub)? {
            members.push(mem);
        }

        Ok(members)
        
    }

    fn member(
        &mut self,
        typ: Option<&Token>,
        allow_pub: bool,
    ) -> Result<Option<UntypedMember>, HayError> {
        let (vis, token) = if allow_pub {
            match self.matches(TokenKind::Keyword(Keyword::Pub)) {
                Ok(t) => {
                    match self.parse_type()? {
                        Some(token) => (Visibility::Public, token),
                        None => return Err(HayError::new(
                            format!("Expected a type after {}, but found {} instead.",
                                Keyword::Pub,
                                self.peek().kind
                            ),
                            t.loc
                        ))
                    }
                },
                Err(_) => match self.parse_type()? {
                    Some(token) => (Visibility::Private, token),
                    None => return Ok(None)
                },
            }
        } else {
            match self.parse_type()? {
                Some(token) => (Visibility::Public, token),
                None => return Ok(None)
            }
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
            parent: typ.cloned(),
            vis,
            token,
            ident,
        }))
    }

    fn function_list(&mut self, impl_on: Option<&Token>) -> Result<Vec<Stmt>, HayError> {
        let mut fns = vec![];
        loop {
            match (
                self.matches(TokenKind::Keyword(Keyword::Function)),
                self.matches(TokenKind::Keyword(Keyword::Inline)),
            ) {
                (Ok(fn_tok), _) => fns.append(&mut self.function(fn_tok, vec![], impl_on)?),
                (_, Ok(inline_tok)) => fns.append(&mut self.inline_function(inline_tok, impl_on)?),
                _ => break,
            }
        }

        Ok(fns)
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

            Ok(Some(self.function_list(Some(impl_on))?))
        } else {
            Ok(None)
        }
    }

    fn if_block(&mut self, token: Token) -> Result<Box<Expr>, HayError> {
        let open = self.tokens.pop().unwrap();
        let then = self.block(open)?;
        let mut otherwise = vec![];
        let mut finally = None;
        while let Ok(else_tok) = self.matches(TokenKind::Keyword(Keyword::Else)) {
            match self.peek().kind {
                TokenKind::Marker(Marker::LeftBrace) => {
                    let open = self.tokens.pop().unwrap();
                    finally = Some(self.block(open)?);
                    break;
                }
                _ => {
                    let cond = self.else_if_condition()?;

                    let open = self.tokens.pop().unwrap();
                    let body = self.block(open)?;

                    otherwise.push(ExprElseIf {
                        token: else_tok,
                        condition: cond,
                        block: *body,
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

        Ok(Box::new(Expr::As(AsExpr {
            token,
            idents,
        })))
    }

    fn var(&mut self, token: Token) -> Result<ExprVar, HayError> {
        let mut typ = match self.parse_type()? {
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
            let lexeme = format!("{kind}");

            typ = Token {
                kind,
                lexeme,
                loc: Loc::new(
                    typ.loc.file,
                    typ.loc.line,
                    typ.loc.span.start,
                    close.loc.span.end,
                ),
            };
        }

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
        while self.matches(TokenKind::Keyword(Keyword::Do)).is_err() {
            cond.push(*self.expression().map_err(|_| HayError::new(
                format!("Expected {} after {} to mark loop body.", Keyword::Do, Keyword::While), 
                token.loc.clone()))?
            );
        }
        let body = self.expression()?;

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
        crate::compiler::test_tools::run_test("tests/parser", "parse_as_block_bad_close", None)
    }

    #[test]
    fn parse_as_block_bad_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_as_block_bad_open", None)
    }

    #[test]
    fn parse_bad_accessor1() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_accessor1", None)
    }

    #[test]
    fn parse_bad_accessor2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_accessor2", None)
    }

    #[test]
    fn parse_bad_accessor3() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_accessor3", None)
    }

    #[test]
    fn parse_bad_annotated_call_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_annotated_call_close", None)
    }

    #[test]
    fn parse_bad_annotated_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_annotated_type", None)
    }

    #[test]
    fn parse_bad_arg_identifier() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_arg_identifier", None)
    }

    #[test]
    fn parse_bad_array_var_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_array_var_close", None)
    }

    #[test]
    fn parse_bad_array_var_size() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_array_var_size", None)
    }

    #[test]
    fn parse_bad_block_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_block_open", None)
    }

    #[test]
    fn parse_bad_body_after_function_name() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_body_after_function_name", None)
    }

    #[test]
    fn parse_bad_cast_expr_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_cast_expr_close", None)
    }

    #[test]
    fn parse_bad_cast_expr_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_cast_expr_open", None)
    }

    #[test]
    fn parse_bad_cast_expr_param() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_cast_expr_param", None)
    }

    #[test]
    fn parse_bad_else_if_block() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_else_if_block", None)
    }

    #[test]
    fn parse_bad_expression() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_expression", None)
    }

    #[test]
    fn parse_bad_file_to_include() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_file_to_include", None)
    }

    #[test]
    fn parse_bad_function_annotation_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_function_annotation_close", None)
    }

    #[test]
    fn parse_bad_function_parameter_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_function_parameter_close", None)
    }

    #[test]
    fn parse_bad_function_return_list_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_function_return_list_close", None)
    }

    #[test]
    fn parse_bad_function_return_list_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_function_return_list_open", None)
    }

    #[test]
    fn parse_bad_include_statement() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_include_statement", None)
    }

    #[test]
    fn parse_bad_pointer_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_pointer_type", None)
    }

    #[test]
    fn parse_bad_top_level_token() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_top_level_token", None)
    }

    #[test]
    fn parse_enum_bad_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_enum_bad_close", None)
    }

    #[test]
    fn parse_enum_bad_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_enum_bad_open", None)
    }

    #[test]
    fn parse_enum_empty_variants() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_enum_empty_variants", None)
    }

    #[test]
    fn parse_enum_without_identifier() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_enum_without_identifier", None)
    }

    #[test]
    fn parse_function_empty_return_list() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_function_empty_return_list", None)
    }

    #[test]
    fn parse_function_without_name() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_function_without_name", None)
    }

    #[test]
    fn parse_mixed_identifier_arg_list() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_mixed_identifier_arg_list", None)
    }

    #[test]
    fn parse_no_args_in_annotated_call() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_no_args_in_annotated_call", None)
    }

    #[test]
    fn parse_struct_bad_annotations_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_struct_bad_annotations_close", None)
    }

    #[test]
    fn parse_struct_bad_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_struct_bad_close", None)
    }

    #[test]
    fn parse_struct_bad_impl_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_struct_bad_impl_open", None)
    }

    #[test]
    fn parse_struct_bad_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_struct_bad_open", None)
    }

    #[test]
    fn parse_struct_empty_members() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_struct_empty_members", None)
    }

    #[test]
    fn parse_struct_member_without_identifier1() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_struct_member_without_identifier1", None)
    }

    #[test]
    fn parse_struct_member_without_identifier2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_struct_member_without_identifier2", None)
    }

    #[test]
    fn parse_struct_pub_member_without_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_struct_pub_member_without_type", None)
    }

    #[test]
    fn parse_struct_without_identifier() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_struct_without_identifier", None)
    }

    #[test]
    fn parse_var_expr_bad_ident() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_var_expr_bad_ident", None)
    }

    #[test]
    fn parse_var_expr_bad_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_var_expr_bad_type", None)
    }

    #[test]
    fn parse_var_expr_missing_colon() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_var_expr_missing_colon", None)
    }

    #[test]
    fn parse_bad_sizeof_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_sizeof_open", None)
    }

    #[test]
    fn parse_bad_sizeof_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_sizeof_close", None)
    }

    #[test]
    fn parse_bad_follow_up_to_inline() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_follow_up_to_inline", None)
    }

    #[test]
    fn parse_on_copy_outside_impl() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_on_copy_outside_impl", None)
    }

    #[test]
    fn parse_on_drop_outside_impl() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_on_drop_outside_impl", None)
    }
    #[test]
    fn parse_bad_on_copy_name() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_on_copy_name", None)
    }
    #[test]
    fn parse_bad_on_drop_name() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_on_drop_name", None)
    }

    #[test]
    fn parse_bad_inner_address_of() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_inner_address_of", None)
    }

    #[test]
    fn parse_bad_const_ptr_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_const_ptr_type", None)
    }

    #[test]
    fn parse_missing_mutable_ident_in_as() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_missing_mutable_ident_in_as", None)
    }

    #[test]
    fn parse_mut_in_fn_output() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_mut_in_fn_output", None)
    }

    #[test]
    fn parse_pub_in_union() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_pub_in_union", None)
    }

    #[test]
    fn parse_partially_named_args() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_partially_named_args", None)
    }

    #[test]
    fn parse_bad_sizeof_operand() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_sizeof_operand", None)
    }

    #[test]
    fn parse_bad_interface_annotation_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_interface_annotation_close", None)
    }

    #[test]
    fn parse_bad_interface_annotation_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_interface_annotation_open", None)
    }

    #[test]
    fn parse_bad_interface_body_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_interface_body_close", None)
    }
    
    #[test]
    fn parse_bad_interface_body_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_interface_body_open", None)
    }

    #[test]
    fn parse_bad_interface_name() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_interface_name", None)
    }

    #[test]
    fn parse_bad_interface_impl_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_interface_impl_close", None)
    }

    #[test]
    fn parse_bad_interface_impl_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_interface_impl_open", None)
    }

    #[test]
    fn parse_bad_interface_impl_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_interface_impl_type", None)
    }

    #[test]
    fn parse_bad_interface_associated_type_name() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_interface_associated_type_name", None)
    }

    #[test]
    fn parse_bad_interface_associated_type_placeholder() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_interface_associated_type_placeholder", None)
    }

    #[test]
    fn parse_bad_interface_associated_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_interface_associated_type", None)
    }
    
    #[test]
    fn parse_bad_requirement_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_requirement_close", None)
    }

    #[test]
    fn parse_bad_requirement_list() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_requirement_list", None)
    }

    #[test]
    fn parse_bad_requirement_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_requirement_open", None)
    }

    #[test]
    fn parse_bad_requirement_start() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_requirement_start", None)
    }

    #[test]
    fn parse_bad_generic_impl_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_generic_impl_close", None)
    }

    #[test]
    fn parse_bad_generic_impl_requires() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_generic_impl_requires", None)
    }

    #[test]
    fn parse_type_missing_associated_type_identifier() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_type_missing_associated_type_identifier", None)
    }

    #[test]
    fn parse_recursive_type() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_recursive_type", None)
    }

    #[test]
    fn parse_tuple_empty() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_tuple_empty", None)
    }

    #[test]
    fn parse_tuple_bad_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_tuple_bad_close", None)
    }
    
    #[test]
    fn parse_bad_tuple_expression_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_bad_tuple_expression_close", None)
    }

    #[test]
    fn parse_match_as_too_many_args() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_match_as_too_many_args", None)
    }

    #[test]
    fn parse_match_bad_as_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_match_bad_as_open", None)
    }

    #[test]
    fn parse_match_bad_as_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_match_bad_as_close", None)
    }

    #[test]
    fn parse_match_bad_open() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_match_bad_open", None)
    }

    #[test]
    fn parse_match_bad_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_match_bad_close", None)
    }

    #[test]
    fn parse_as_block_bad_destructure_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_as_block_bad_destructure_close", None)
    }

    #[test]
    fn parse_as_block_empty_destructure() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_as_block_empty_destructure", None)
    }

    #[test]
    fn anon_struct_bad_close() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "anon_struct_bad_close", None)
    }

    #[test]
    fn parse_while_without_do() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("tests/parser", "parse_while_without_do", None)
    }

}
