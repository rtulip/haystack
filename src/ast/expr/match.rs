use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        arg::{IdentArg, IdentArgKind},
        expr::{AccessorExpr, AsExpr, ExprElseIf, ExprIf, ExprLiteral, ExprOperator},
        member::TypedMember,
        stmt::GlobalEnv,
    },
    error::HayError,
    lex::token::{Keyword, Literal, Operator, Token, TokenKind},
    types::{Frame, RecordKind, Stack, Type, TypeId, TypeMap, UncheckedFunction, VariantType},
};

use super::{BlockExpr, Expr, TypedExpr};

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
        self,
        stack: &mut Stack,
        frame: &mut Frame,
        func: &UncheckedFunction,
        global_env: &mut GlobalEnv,
        types: &mut TypeMap,
        generic_map: &Option<HashMap<TypeId, TypeId>>,
    ) -> Result<TypedExpr, HayError> {
        let base_tid = match stack.last() {
            Some(t) => t.clone(),
            None => {
                return Err(HayError::new(
                    format!(
                        "{} expects one element on the stack. Found none.",
                        Keyword::Match
                    ),
                    self.token.loc,
                ))
            }
        };

        let (base_tid, base_variants) = match types.get(&base_tid) {
            Some(Type::Record {
                members,
                kind: RecordKind::EnumStruct,
                ..
            }) => (base_tid, members.clone()),
            Some(Type::Variant(VariantType { base, .. })) => match types.get(base) {
                Some(Type::Record {
                    members,
                    kind: RecordKind::EnumStruct,
                    ..
                }) => (base.clone(), members.clone()),
                t => unreachable!("Internal Error: Expected an EnumStruct. Found {t:?}"),
            },
            Some(_) => {
                return Err(HayError::new(
                    format!(
                        "{} expects an `{}`, but found `{}` instead",
                        Keyword::Match,
                        RecordKind::EnumStruct,
                        base_tid
                    ),
                    self.token.loc,
                ))
            }
            _ => unreachable!(
                "Internal error: Type {base_tid} was on the stack, but unknown to type system."
            ),
        };

        let mut ident_name = String::from("0");
        for (framed_id, _) in frame.iter() {
            ident_name += framed_id;
        }

        let ident = Token {
            kind: TokenKind::Ident(ident_name.clone()),
            lexeme: ident_name,
            loc: self.token.loc.clone(),
        };

        if !self.cases.is_empty() {
            let mut cases_handled = HashSet::new();

            let (idx, mut before_exprs, then_exprs) =
                self.exprs_from_case(&self.cases[0], &ident, &base_tid, &base_variants, types)?;
            cases_handled.insert(idx);

            let mut otherwise_exprs = vec![];

            for case in &self.cases[1..] {
                let (idx, before, otherwise) =
                    self.exprs_from_case(case, &ident, &base_tid, &base_variants, types)?;

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

            if cases_handled.len() != base_variants.len() && self.else_case.is_none() {
                let mut e = HayError::new(
                    format!(
                        "Match expression doesn't handle all cases for enum-struct `{base_tid}`"
                    ),
                    self.token.loc,
                )
                .with_hint("The following cases are not handled:");

                for i in (0..base_variants.len()).filter(|idx| !cases_handled.contains(idx)) {
                    e = e.with_hint(format!(" - {base_tid}::{}", base_variants[i].ident.lexeme));
                }

                return Err(e);
            }

            let finally = self
                .else_case
                .map(|case| case.body)
                .unwrap_or(Box::new(Expr::Block(BlockExpr {
                    open: self.token.clone(),
                    close: self.token.clone(),
                    exprs: vec![Expr::Never(super::NeverExpr {
                        token: self.token.clone(),
                    })],
                })));

            let if_expr = Expr::If(ExprIf {
                token: self.token.clone(),
                then: Box::new(Expr::Block(then_exprs)),
                otherwise: else_if_exprs,
                finally: Some(finally),
            });

            before_exprs.exprs.push(if_expr);

            let as_expr = Expr::As(AsExpr {
                token: self.token.clone(),
                idents: vec![IdentArg {
                    kind: IdentArgKind::Single {
                        token: ident.clone(),
                    },
                    mutable: None,
                }],
                block: Some(Box::new(Expr::Block(before_exprs))),
            });
            as_expr.type_check(stack, frame, func, global_env, types, generic_map)
        } else if let Some(else_case) = self.else_case {
            let as_expr = Expr::As(AsExpr {
                token: self.token.clone(),
                idents: vec![IdentArg {
                    kind: IdentArgKind::Single {
                        token: Token {
                            kind: TokenKind::Ident(String::from("0")),
                            lexeme: String::from("0"),
                            loc: self.token.loc.clone(),
                        },
                    },
                    mutable: None,
                }],
                block: Some(else_case.body),
            });
            as_expr.type_check(stack, frame, func, global_env, types, generic_map)
        } else {
            let mut e = HayError::new(
                format!("Empty match block handles no cases of enum-struct `{base_tid}`"),
                self.token.loc,
            )
            .with_hint("The following cases were not handled:");

            for variant in base_variants {
                e = e.with_hint(format!(" - {base_tid}::{}", variant.ident.lexeme))
            }

            Err(e)
        }
    }

    fn exprs_from_case(
        &self,
        case: &MatchCaseExpr,
        ident_tok: &Token,
        base_tid: &TypeId,
        base_variants: &[TypedMember],
        types: &mut TypeMap,
    ) -> Result<(usize, BlockExpr, BlockExpr), HayError> {
        let idx = self.find_variant_index(&case.variant, base_variants, base_tid, types)?;

        let before_exprs = vec![
            Expr::Accessor(AccessorExpr {
                token: case.variant.clone(),
                ident: ident_tok.clone(),
                inner: vec![],
            }),
            Expr::Literal(ExprLiteral {
                literal: Literal::U64(idx as u64),
                token: case.variant.clone(),
            }),
            Expr::Operator(ExprOperator {
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
                block: None,
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

    fn find_variant_index(
        &self,
        variant: &Token,
        base_variants: &[TypedMember],
        base_tid: &TypeId,
        types: &mut TypeMap,
    ) -> Result<usize, HayError> {
        let tid = TypeId::from_token(variant, types, &vec![])?;

        match types.get(&tid) {
            Some(Type::Variant(VariantType {
                base,
                variant: variant_str,
            })) => {
                if base_tid != base && &base_tid.supertype(types) != base {
                    return Err(HayError::new(
                        format!("{tid} is not a variant of {base_tid}"),
                        variant.loc.clone(),
                    ));
                }

                match base_variants
                    .iter()
                    .enumerate()
                    .find(|(_, m)| &m.ident.lexeme == variant_str)
                {
                    Some((i, _)) => Ok(i),
                    None => unreachable!(
                        "Internal Error: Unknown variant `{variant_str}` for base `{base}`"
                    ),
                }
            }
            _ => Err(HayError::new(
                format!(
                    "{} case expected a variant of `{base_tid}`.",
                    Keyword::Match
                ),
                variant.loc.clone(),
            )
            .with_hint(format!("Found type `{tid}` instead."))),
        }
    }
}
