use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        arg::{IdentArg, IdentArgKind},
        expr::{AccessorExpr, AsExpr, ExprElseIf, IfExpr, LiteralExpr, OperatorExpr},
        member::TypedMember,
        stmt::{Functions, GlobalVars, InterfaceFunctionTable, Interfaces, UserDefinedTypes},
    },
    error::HayError,
    lex::token::{Keyword, Literal, Operator, Token, TokenKind},
    types::{Frame, FreeVars, RecordKind, RecordType, Stack, Substitutions, Type, VariantType},
};

use super::{BlockExpr, Expr};

#[derive(Debug, Clone)]
pub struct MatchExpr {
    pub token: Token,
    pub cases: Vec<MatchCaseExpr>,
    pub else_case: Option<MatchElseExpr>,
}

#[derive(Debug, Clone)]
pub struct MatchCaseExpr {
    pub variant: Token,
    pub ident: Option<IdentArg>,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct MatchElseExpr {
    pub token: Token,
    pub body: Box<Expr>,
}

impl MatchExpr {
    pub fn type_check(
        &self,
        stack: &mut Stack,
        frame: &mut Frame,
        user_defined_types: &UserDefinedTypes,
        global_vars: &GlobalVars,
        functions: &Functions,
        interfaces: &Interfaces,
        interface_fn_table: &InterfaceFunctionTable,
        subs: &mut Substitutions,
        free_vars: &FreeVars,
    ) -> Result<(), HayError> {
        let typ = match stack.last() {
            Some(t) => t.clone(),
            None => {
                return Err(HayError::new(
                    format!(
                        "{} expects one element on the stack. Found none.",
                        Keyword::Match
                    ),
                    self.token.loc.clone(),
                ))
            }
        };

        let record = match &typ {
            Type::Record(record) if &record.kind == &RecordKind::EnumStruct => record.clone(),
            Type::Variant(VariantType {
                typ: box Type::Record(record),
                ..
            }) if &record.kind == &RecordKind::EnumStruct => record.clone(),
            _ => {
                return Err(HayError::new(
                    format!(
                        "{} expects an `{}`, but found `{typ}` instead",
                        Keyword::Match,
                        RecordKind::EnumStruct,
                    ),
                    self.token.loc.clone(),
                ))
            }
        };

        let mut ident_name = String::from("0");
        for (framed_id, _) in frame.iter() {
            ident_name += framed_id;
        }

        let ident_tok = Token {
            kind: TokenKind::Ident(ident_name.clone()),
            lexeme: ident_name,
            loc: self.token.loc.clone(),
        };

        if !self.cases.is_empty() {
            // Track which cases have been matched for exhaustiveness checking.
            let mut cases_handled = HashSet::new();

            // Turn the match expression into a block.
            // Start by binding the enum-struct to an identifier with an `as`
            // expression.
            let mut match_block = BlockExpr {
                open: self.token.clone(),
                close: self.token.clone(),
                exprs: vec![Expr::As(AsExpr {
                    token: self.token.clone(),
                    idents: vec![IdentArg {
                        kind: IdentArgKind::Single {
                            token: ident_tok.clone(),
                        },
                        mutable: None,
                    }],
                })],
            };

            let (idx, before_exprs, then_exprs) = self.exprs_from_case(
                &self.cases[0],
                &ident_tok,
                &record,
                user_defined_types,
                free_vars,
            )?;
            cases_handled.insert(idx);
            match_block.exprs.push(Expr::Block(before_exprs));

            let mut otherwise_exprs = vec![];

            for case in &self.cases[1..] {
                let (idx, before, otherwise) =
                    self.exprs_from_case(case, &ident_tok, &record, user_defined_types, free_vars)?;

                cases_handled.insert(idx);
                otherwise_exprs.push((before, otherwise));
            }

            let else_if_exprs = otherwise_exprs
                .into_iter()
                .map(|(condition, block)| ExprElseIf {
                    token: self.token.clone(),
                    condition: condition.exprs,
                    block: Expr::Block(block),
                })
                .collect();

            if cases_handled.len() != record.members.len() && self.else_case.is_none() {
                let mut e = HayError::new(
                    format!("Match expression doesn't handle all cases for enum-struct `{typ}`"),
                    self.token.loc.clone(),
                )
                .with_hint("The following cases are not handled:");

                for i in (0..record.members.len()).filter(|idx| !cases_handled.contains(idx)) {
                    e = e.with_hint(format!(" - {typ}::{}", record.members[i].ident));
                }

                return Err(e);
            }

            let finally = self
                .else_case
                .as_ref()
                .map(|case| case.body.clone())
                .unwrap_or(Box::new(Expr::Block(BlockExpr {
                    open: self.token.clone(),
                    close: self.token.clone(),
                    exprs: vec![Expr::Never(super::NeverExpr {
                        token: self.token.clone(),
                    })],
                })));

            let if_expr = Expr::If(IfExpr {
                token: self.token.clone(),
                then: Box::new(Expr::Block(then_exprs)),
                otherwise: else_if_exprs,
                finally: Some(finally.clone()),
            });

            match_block.exprs.push(if_expr);

            let match_expr = Expr::Block(match_block);

            println!("Type Checking match expr");

            match_expr.type_check(
                stack,
                frame,
                user_defined_types,
                global_vars,
                functions,
                interfaces,
                interface_fn_table,
                subs,
            )
        } else if let Some(else_case) = &self.else_case {
            todo!()
            // let match_block = BlockExpr {
            //     open: self.token.clone(),
            //     close: self.token.clone(),
            //     exprs: vec![
            //         Expr::As(AsExpr {
            //             token: self.token.clone(),
            //             idents: vec![IdentArg {
            //                 kind: IdentArgKind::Single {
            //                     token: Token {
            //                         kind: TokenKind::Ident(String::from("0")),
            //                         lexeme: String::from("0"),
            //                         loc: self.token.loc.clone(),
            //                     },
            //                 },
            //                 mutable: None,
            //             }],
            //         }),
            //         *else_case.body,
            //     ],
            // };

            // match_block.type_check(stack, frame, func, global_env, types, generic_map)
        } else {
            let mut e = HayError::new(
                format!("Empty match block handles no cases of enum-struct `{typ}`"),
                self.token.loc.clone(),
            )
            .with_hint("The following cases were not handled:");

            for variant in &record.members {
                e = e.with_hint(format!(" - {}::{}", variant.typ, variant.ident))
            }

            Err(e)
        }
    }

    /// Expressions from case.
    ///
    /// I don't have a great way to do syntactic sugar at the moment, so it's
    /// code manipulation is done manually for now.
    ///
    /// This function takes a `match` case, and converts it into if-else cases.
    ///
    /// For example:
    /// ```
    /// enum struct Foo {
    ///     u64: Bar
    ///     Str: Baz
    /// }
    ///
    /// fn Quxx(Foo) {
    ///     match {
    ///         Foo::Bar { 1 println }
    ///         Foo::Baz as [s] { s println }
    ///     }
    /// }
    /// ```
    ///
    /// Gets converted into:
    /// ```
    /// fn Quxx(Foo) {
    ///     as [foo]
    ///     foo::discriminant Foo::Bar::discriminant == if {
    ///         1 println
    ///     } else foo::discriminant Foo::Bar::discriminant == if {
    ///         foo::1 as [s]
    ///         s println
    ///     } else {
    ///         unreachable
    ///     }
    /// }
    /// ```
    ///
    /// This function takes a case `Foo::Bar { 1 println }` and converts it
    /// into two code blocks: one which checks the discriminant and run before
    /// the `if` statement, and the other is the body of the if statement. The
    /// identifier is optionally added to the scope in the second block as well.
    ///
    /// So from our example, the case `Foo::Bar as [s] { s println }` gets
    /// transformed into:
    /// ```
    /// idx = 1
    /// before_exprs = { foo::discriminant Foo::Bar::discriminant == }
    /// then_exprs = { foo::1 as [s] { s println } }
    /// ```
    fn exprs_from_case(
        &self,
        case: &MatchCaseExpr,
        ident_tok: &Token,
        record: &RecordType,
        user_defined_types: &UserDefinedTypes,
        free_vars: &FreeVars,
    ) -> Result<(usize, BlockExpr, BlockExpr), HayError> {
        let idx = match Type::from_token(&case.variant, user_defined_types, free_vars)? {
            Type::Variant(VariantType {
                variant,
                typ: box Type::Record(ref record_variant),
            }) if &record.ident == &record_variant.ident => {
                if let Some((idx, _)) = record_variant
                    .members
                    .iter()
                    .enumerate()
                    .find(|(_, member)| &member.ident == &variant)
                {
                    idx
                } else {
                    todo!()
                }
            }
            _ => todo!(),
        };

        println!("IDX: {idx}");

        // Case Descriminant ==
        let before_exprs = vec![
            Expr::Accessor(AccessorExpr {
                token: case.variant.clone(),
                ident: ident_tok.clone(),
                inner: vec![],
            }),
            Expr::Literal(LiteralExpr {
                literal: Literal::U64(idx as u64),
                token: case.variant.clone(),
            }),
            Expr::Operator(OperatorExpr {
                op: Operator::Equal,
                token: case.variant.clone(),
            }),
        ];

        let mut then_exprs = vec![];

        if let Some(ident) = &case.ident {
            then_exprs.push(Expr::Accessor(AccessorExpr {
                token: case.variant.clone(),
                ident: ident_tok.clone(),
                inner: vec![Token {
                    kind: TokenKind::Literal(Literal::U64(idx as u64)),
                    lexeme: format!("{idx}"),
                    loc: self.token.loc.clone(),
                }],
            }));

            then_exprs.push(Expr::As(AsExpr {
                token: self.token.clone(),
                idents: vec![ident.clone()],
            }));
        }

        then_exprs.push(case.body.clone());

        Ok((
            idx,
            BlockExpr {
                open: self.token.clone(),
                close: self.token.clone(),
                exprs: before_exprs,
            },
            BlockExpr {
                open: self.token.clone(),
                close: self.token.clone(),
                exprs: then_exprs,
            },
        ))
    }
}
