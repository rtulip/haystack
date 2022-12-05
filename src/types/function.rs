use std::collections::HashMap;

use crate::{
    ast::{
        arg::TypedArg,
        expr::{Expr, TypedExpr},
        stmt::GlobalEnv,
    },
    error::HayError,
    lex::token::Token,
    types::TypeId,
};

use super::{FramedType, Type, TypeMap};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum FnTag {
    Inline,
    Interface(TypeId),
    OnCopy,
    OnDrop,
}

#[derive(Debug, Clone)]
pub struct UncheckedFunction {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<TypedArg>,
    pub outputs: Vec<TypedArg>,
    pub body: Vec<Expr>,
    pub generic_map: Option<HashMap<TypeId, TypeId>>,
    pub tags: Vec<FnTag>,
    pub impl_on: Option<TypeId>,
}

#[derive(Debug, Clone)]
pub struct GenericFunction {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<TypedArg>,
    pub outputs: Vec<TypedArg>,
    pub generics: Vec<TypeId>,
    pub body: Vec<Expr>,
    pub tags: Vec<FnTag>,
    pub impl_on: Option<TypeId>,
    pub requires: Option<Vec<Token>>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<TypedArg>,
    pub outputs: Vec<TypedArg>,
    pub body: Vec<TypedExpr>,
    pub generic_map: Option<HashMap<TypeId, TypeId>>,
    pub tags: Vec<FnTag>,
}

#[derive(Debug, Clone)]
pub struct FunctionStub {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<TypedArg>,
    pub outputs: Vec<TypedArg>,
    pub generics: Option<Vec<TypeId>>,
    pub tags: Vec<FnTag>,
    pub impl_on: Option<TypeId>,
}

impl Function {
    pub fn has_tag(&self, tag: FnTag) -> bool {
        self.tags.contains(&tag)
    }
}

impl UncheckedFunction {
    pub fn type_check(
        &self,
        global_env: &mut GlobalEnv,
        types: &mut TypeMap,
    ) -> Result<Function, HayError> {
        let mut stack = vec![];
        let mut frame = vec![];

        if self.inputs.first().is_some() && self.inputs.first().unwrap().ident.is_some() {
            self.inputs.iter().rev().for_each(|arg| {
                frame.push((
                    arg.ident.as_ref().unwrap().lexeme.clone(),
                    FramedType {
                        origin: arg.token.clone(),
                        typ: arg.typ.clone(),
                        mutable: arg.mutable.is_some(),
                    },
                ))
            });
        } else {
            self.inputs
                .iter()
                .for_each(|arg| stack.push(arg.typ.clone()));
        }

        let mut typed_body = vec![];
        for expr in self.body.clone() {
            typed_body.push(expr.type_check(
                &mut stack,
                &mut frame,
                self,
                global_env,
                types,
                &self.generic_map,
            )?);
        }
        let stack_tids = stack.iter().collect::<Vec<&TypeId>>();
        let output_tids = self
            .outputs
            .iter()
            .map(|arg| &arg.typ)
            .collect::<Vec<&TypeId>>();

        if !stack_tids.contains(&&Type::Never.id()) && stack_tids != output_tids {
            return Err(HayError::new_type_err(
                format!(
                    "Function `{}` doesn't produce the correct outputs",
                    self.name.lexeme
                ),
                self.name.loc.clone(),
            )
            .with_hint(format!("Expected final stack: {output_tids:?}"))
            .with_hint(format!("Function produced:    {stack_tids:?}")));
        }

        Ok(Function {
            token: self.token.clone(),
            name: self.name.clone(),
            inputs: self.inputs.clone(),
            outputs: self.outputs.clone(),
            body: typed_body,
            generic_map: self.generic_map.clone(),
            tags: self.tags.clone(),
        })
    }
}
