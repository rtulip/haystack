use crate::{
    error::HayError,
    lex::token::{Keyword, Operator},
    types::{Frame, FramedType, RecordKind, Stack, Type, TypeMap, UncheckedFunction},
};

use super::{AccessorExpr, Expr, ExprIdent, ExprOperator, TypedExpr};

#[derive(Debug, Clone)]
pub struct ExprUnary {
    pub op: ExprOperator,
    pub expr: Box<Expr>,
}

impl ExprUnary {
    pub fn type_check(
        self,
        stack: &mut Stack,
        frame: &mut Frame,
        types: &mut TypeMap,
        func: &UncheckedFunction,
    ) -> Result<TypedExpr, HayError> {
        match (&self.op.op, *self.expr) {
            (
                Operator::Ampersand | Operator::Star,
                Expr::Accessor(AccessorExpr { ident, inner, .. }),
            ) => {
                match frame
                    .iter()
                    .enumerate()
                    .find(|(_, (id, _))| &ident.lexeme == id)
                {
                    Some((
                        idx,
                        (
                            _,
                            FramedType {
                                origin,
                                typ,
                                mutable,
                            },
                        ),
                    )) => {
                        if matches!((&self.op.op, mutable), (Operator::Star, false)) {
                            return Err(HayError::new_type_err(
                                format!(
                                    "Cannot take mutable reference to immutable ident: `{}`",
                                    ident.lexeme
                                ),
                                self.op.token.loc,
                            )
                            .with_hint_and_custom_note(
                                format!(
                                    "Consider adding {} in binding of `{}`",
                                    Keyword::Mut,
                                    &ident.lexeme
                                ),
                                format!("{}", &origin.loc),
                            ));
                        }

                        let mut typ = typ;
                        for inner_member in &inner {
                            if let Type::Record {
                                name,
                                members,
                                kind,
                                ..
                            } = types.get(typ).unwrap()
                            {
                                if let Some(m) = members
                                    .iter()
                                    .find(|m| m.ident.lexeme == inner_member.lexeme)
                                {
                                    if !m.is_public() {
                                        match &func.impl_on {
                                    Some(typ) => if &m.parent != typ {
                                        return Err(
                                            HayError::new_type_err(
                                                format!("Cannot access {kind} `{}` member `{}` as it is declared as private.", name.lexeme, m.ident.lexeme),
                                                self.op.token.loc
                                            ).with_hint_and_custom_note(format!("{kind} `{}` declared here", name.lexeme), format!("{}", name.loc))
                                        )
                                    }
                                    _ => return Err(
                                        HayError::new_type_err(
                                            format!("Cannot access {kind} `{}` member `{}` as it is declared as private.", name.lexeme, m.ident.lexeme),
                                            self.op.token.loc
                                        ).with_hint_and_custom_note(format!("{kind} `{}` declared here", name.lexeme), format!("{}", name.loc))
                                    )
                                }
                                    }

                                    typ = &m.typ;
                                } else {
                                    return Err(HayError::new_type_err(
                                        format!(
                                            "{} `{}` doesn't have a member `{}`",
                                            match kind {
                                                RecordKind::Union => "Union",
                                                RecordKind::Struct => "Struct",
                                                RecordKind::EnumStruct => "Enum struct",
                                                RecordKind::Interface => unreachable!(),
                                            },
                                            name.lexeme,
                                            inner_member.lexeme,
                                        ),
                                        self.op.token.loc,
                                    )
                                    .with_hint(format!(
                                        "`{}` has the following members: {:?}",
                                        name.lexeme,
                                        members
                                            .iter()
                                            .map(|m| &m.ident.lexeme)
                                            .collect::<Vec<&String>>()
                                    )));
                                }
                            } else {
                                return Err(HayError::new(
                                    format!("Cannot access into non-record type `{typ}`"),
                                    self.op.token.loc,
                                ));
                            }
                        }

                        stack.push(typ.clone().ptr_of(
                            match self.op.op {
                                Operator::Star => true,
                                Operator::Ampersand => false,
                                _ => unreachable!(),
                            },
                            types,
                        ));

                        Ok(TypedExpr::AddrFramed {
                            frame: frame.clone(),
                            idx,
                            inner: if inner.is_empty() {
                                None
                            } else {
                                Some(inner.iter().map(|t| t.lexeme.clone()).collect())
                            },
                        })
                    }
                    None => Err(HayError::new_type_err(
                        format!("Can't take address of unknown identifier: `{ident}`"),
                        self.op.token.loc,
                    )),
                }
            }
            (Operator::Ampersand | Operator::Star, Expr::Ident(ExprIdent { ident })) => match frame
                .iter()
                .enumerate()
                .find(|(_, (id, _))| &ident.lexeme == id)
            {
                Some((
                    idx,
                    (
                        _,
                        FramedType {
                            origin,
                            typ,
                            mutable,
                        },
                    ),
                )) => {
                    if matches!((&self.op.op, mutable), (Operator::Star, false)) {
                        return Err(HayError::new_type_err(
                            format!(
                                "Cannot take mutable reference to immutable ident: `{}`",
                                ident.lexeme
                            ),
                            self.op.token.loc,
                        )
                        .with_hint_and_custom_note(
                            format!(
                                "Consider adding {} in binding of `{}`",
                                Keyword::Mut,
                                &ident.lexeme
                            ),
                            format!("{}", &origin.loc),
                        ));
                    }

                    stack.push(typ.clone().ptr_of(
                        match self.op.op {
                            Operator::Star => true,
                            Operator::Ampersand => false,
                            _ => unreachable!(),
                        },
                        types,
                    ));

                    Ok(TypedExpr::AddrFramed {
                        frame: frame.clone(),
                        idx,
                        inner: None,
                    })
                }
                None => Err(HayError::new_type_err(
                    format!(
                        "Can't take address of unknown identifier: `{}`",
                        ident.lexeme
                    ),
                    self.op.token.loc,
                )),
            },
            _ => unimplemented!(),
        }
    }
}
