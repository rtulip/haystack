use std::collections::HashMap;

use crate::{
    ast::{
        arg::TypedArg,
        expr::{Expr, TypedExpr},
        stmt::GlobalEnv,
    },
    error::HayError,
    lex::token::Token,
    types::{stack_compare_exact, TypeId},
};

use super::{FramedType, Type, TypeMap, Variance};

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
    pub body: Expr,
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
    pub body: Expr,
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
    pub body: TypedExpr,
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

        let typed_body = self.body.clone().type_check(
            &mut stack,
            &mut frame,
            self,
            global_env,
            types,
            &self.generic_map,
        )?;

        // let stack_tids = stack.iter().collect::<Vec<&TypeId>>();
        let output_tids = self
            .outputs
            .iter()
            .map(|arg| arg.typ.clone())
            .collect::<Vec<TypeId>>();

        // Don't report mismatched outputs if the function will never reach the end
        if !stack.contains(&Type::Never.id()) {
            // If the function explicitly never returns, the following must be true:
            // 1. The stack is empty
            // 2. The output only contains the never type.
            if !(stack.is_empty() && output_tids.len() == 1 && output_tids[0] == Type::Never.id()) {
                // If the stack lengths aren't the same, and the stack isn't
                // covariant to the output, then report an error.
                if !stack_compare_exact(&output_tids, &stack, types, Variance::Covariant) {
                    return Err(HayError::new_type_err(
                        format!(
                            "Function `{}` doesn't produce the correct outputs",
                            self.name.lexeme
                        ),
                        self.name.loc.clone(),
                    )
                    .with_hint(format!("Expected final stack: {output_tids:?}"))
                    .with_hint(format!("Function produced:    {stack:?}")));
                }
            }
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
