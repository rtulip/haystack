use std::collections::BTreeMap;

use crate::{
    ast::{arg::UntypedArg, expr::Expr},
    error::HayError,
    lex::token::Token,
    types::FunctionType,
};

use super::InterfaceId;

#[derive(Debug, Clone)]
pub enum FnTag {
    Interface(InterfaceId),
    Inline,
}

#[derive(Debug, Clone)]
pub struct FunctionStmt {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<UntypedArg>,
    pub outputs: Vec<UntypedArg>,
    pub annotations: Option<Vec<UntypedArg>>,
    pub body: Expr,
    pub tags: Vec<FnTag>,
    pub impl_on: Option<Token>,
    pub requires: Option<Vec<Token>>,
}

pub struct FunctionDescription {
    pub name: Token,
    pub typ: FunctionType,
    pub body: Expr,
    pub tags: Vec<FnTag>,
}

impl FunctionStmt {
    pub fn add_to_global_env(
        self,
        functions: &mut BTreeMap<String, FunctionDescription>,
    ) -> Result<(), HayError> {
        let inputs = UntypedArg::into_typed_args(self.inputs)?;
        let outputs = UntypedArg::into_typed_args(self.outputs)?;

        let function_type = FunctionType::from_typed_args(&inputs, &outputs);

        if self.annotations.is_some() {
            todo!()
        }
        if self.impl_on.is_some() {
            todo!()
        }
        if self.requires.is_some() {
            todo!()
        }

        if functions.contains_key(&self.name.lexeme) {
            todo!()
        }

        functions.insert(
            self.name.lexeme.clone(),
            FunctionDescription {
                name: self.name,
                typ: function_type,
                body: self.body,
                tags: self.tags,
            },
        );

        Ok(())
    }
}
