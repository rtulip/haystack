use std::collections::HashMap;

use crate::{
    ast::{
        arg::IdentArg,
        expr::{AccessorExpr, AsExpr, ExprElseIf, ExprIf, ExprLiteral, ExprOperator},
        member::TypedMember,
        stmt::GlobalEnv,
    },
    error::HayError,
    lex::token::{Keyword, Literal, Operator, Token, TokenKind},
    types::{Frame, RecordKind, Stack, Type, TypeId, TypeMap, UncheckedFunction, VariantType},
};

use super::{Expr, TypedExpr};

#[derive(Debug, Clone)]
pub struct MatchExpr {
    pub token: Token,
    pub cases: Vec<MatchCaseExpr>,
}

#[derive(Debug, Clone)]
pub struct MatchCaseExpr {
    pub variant: Token,
    pub ident: Option<IdentArg>,
    pub body: Vec<Expr>,
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
            Some(Type::Variant(VariantType { base, .. })) => match types.get(&base) {
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

        let mut cases_handled = vec![];

        let (idx, mut before_exprs, then_exprs) =
            self.exprs_from_case(&self.cases[0], &base_tid, &base_variants, types)?;
        cases_handled.push(idx);

        let mut otherwise_exprs = vec![];

        for case in &self.cases[1..] {
            let (idx, before, otherwise) =
                self.exprs_from_case(case, &base_tid, &base_variants, types)?;

            cases_handled.push(idx);
            otherwise_exprs.push((before, otherwise));
        }

        let else_if_exprs = otherwise_exprs
            .into_iter()
            .map(|(condition, block)| ExprElseIf {
                token: self.token.clone(),
                condition,
                block,
            })
            .collect();

        let if_expr = Expr::If(ExprIf {
            token: self.token.clone(),
            then: then_exprs,
            otherwise: else_if_exprs,
            finally: None,
        });

        before_exprs.push(if_expr);

        let as_expr = Expr::As(AsExpr {
            token: self.token.clone(),
            idents: vec![IdentArg {
                token: Token {
                    kind: TokenKind::Ident(String::from("0")),
                    lexeme: String::from("0"),
                    loc: self.token.loc.clone(),
                },
                mutable: None,
            }],
            block: Some(before_exprs),
        });

        as_expr.type_check(stack, frame, func, global_env, types, generic_map)
    }

    fn exprs_from_case(
        &self,
        case: &MatchCaseExpr,
        base_tid: &TypeId,
        base_variants: &Vec<TypedMember>,
        types: &mut TypeMap,
    ) -> Result<(usize, Vec<Expr>, Vec<Expr>), HayError> {
        let idx = self.find_variant_index(&case.variant, base_variants, base_tid, types)?;

        let mut before_exprs = vec![];
        before_exprs.push(Expr::Accessor(AccessorExpr {
            token: case.variant.clone(),
            ident: Token {
                kind: TokenKind::Ident(String::from("0")),
                lexeme: String::from("0"),
                loc: case.variant.loc.clone(),
            },
            inner: vec![],
        }));

        before_exprs.push(Expr::Literal(ExprLiteral {
            literal: Literal::U64(idx as u64),
            token: case.variant.clone(),
        }));

        before_exprs.push(Expr::Operator(ExprOperator {
            op: Operator::Equal,
            token: case.variant.clone(),
        }));

        let mut then_exprs = vec![];

        if let Some(ident) = &case.ident {
            then_exprs.push(Expr::Accessor(AccessorExpr {
                token: case.variant.clone(),
                ident: Token {
                    kind: TokenKind::Ident(String::from("0")),
                    lexeme: String::from("0"),
                    loc: ident.token.loc.clone(),
                },
                inner: vec![Token {
                    kind: TokenKind::Literal(Literal::U64(idx as u64)),
                    lexeme: format!("{idx}"),
                    loc: ident.token.loc.clone(),
                }],
            }));

            then_exprs.push(Expr::As(AsExpr {
                token: ident.token.clone(),
                idents: vec![ident.clone()],
                block: None,
            }));
        }

        then_exprs.append(&mut case.body.clone());

        Ok((idx, before_exprs, then_exprs))
    }

    fn find_variant_index(
        &self,
        variant: &Token,
        base_variants: &Vec<TypedMember>,
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
            _ => {
                return Err(HayError::new(
                    format!(
                        "{} case expected a variant of `{base_tid}`.",
                        Keyword::Match
                    ),
                    variant.loc.clone(),
                )
                .with_hint(format!("Found type `{tid}` instead.")))
            }
        }
    }
}
