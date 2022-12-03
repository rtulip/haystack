use std::collections::HashMap;

use crate::{
    ast::{
        expr::ExprIdent,
        stmt::{GlobalEnv, StmtKind},
    },
    error::HayError,
    lex::token::Token,
};

use super::{Stack, Type, TypeId, TypeMap};

#[derive(Debug, Clone)]
pub struct InterfaceBaseType {
    pub token: Token,
    pub name: Token,
    pub annotations: Vec<TypeId>,
    pub types: HashMap<TypeId, Token>,
    pub fns: Vec<String>,
    pub requires: Option<Vec<Token>>,
    pub impls: Vec<TypeId>,
}

#[derive(Debug, Clone)]
pub struct InterfaceInstanceType {
    pub token: Token,
    pub mapping: Vec<TypeId>,
    pub fns_map: HashMap<TypeId, TypeId>,
}

impl InterfaceBaseType {
    pub fn id(&self) -> TypeId {
        TypeId::new(&self.name.lexeme)
    }

    pub fn full_id(&self) -> TypeId {
        let mut name = format!("{}<", self.name.lexeme);
        for ann in &self.annotations[0..self.annotations.len() - 1] {
            name = format!("{name}{ann} ");
        }
        name = format!("{name}{}>", self.annotations.last().unwrap());
        TypeId::new(name)
    }

    pub fn resolve(
        &self,
        expr: &ExprIdent,
        stack: &mut Stack,
        types: &mut TypeMap,
        global_env: &GlobalEnv,
    ) -> Result<String, HayError> {
        let mut err = HayError::new_type_err(
            format!(
                "Failed to resolve interface function `{}`",
                &expr.ident.lexeme
            ),
            expr.ident.loc.clone(),
        )
        .with_hint(format!("Interface `{}` is implemented by:", self.full_id(),));

        for instance in &self.impls {
            let mapped_fn = match types.get(instance) {
                Some(Type::InterfaceInstance(instance)) => {
                    err = err.with_hint(format!("  {}", instance.token.lexeme));

                    let fn_tid = TypeId::new(&expr.ident.lexeme);
                    instance.fns_map.get(&fn_tid).unwrap().clone()
                }
                _ => unreachable!(),
            };

            match global_env.get(&mapped_fn.0).unwrap() {
                (StmtKind::Function, signature) => {
                    if signature.evaluate(&expr.ident, stack, types).is_ok() {
                        return Ok(mapped_fn.0);
                    }
                }
                _ => unreachable!(),
            }
        }

        let (_, interface_sig) = global_env.get(&expr.ident.lexeme).unwrap();
        Err(err
            .with_hint(format!("Function `{}` expected:", &expr.ident.lexeme))
            .with_hint(format!("  {:?}", interface_sig.inputs))
            .with_hint("Found:")
            .with_hint(format!(
                "  {:?}",
                stack
                    .iter()
                    .rev()
                    .take(interface_sig.inputs.len())
                    .rev()
                    .collect::<Vec<&TypeId>>()
            )))
    }

    pub fn find_impl(&self, token: &Token, map: &HashMap<TypeId, TypeId>) -> Result<(), HayError> {
        let mut impl_name = format!("{}<", self.name.lexeme);
        for ann in &self.annotations[0..self.annotations.len() - 1] {
            impl_name = format!(
                "{impl_name}{} ",
                map.get(ann)
                    .expect("Should have found a mapping for annotation")
            );
        }
        impl_name = format!(
            "{impl_name}{}>",
            map.get(self.annotations.last().unwrap())
                .expect("Should have found a mapping for the annotations")
        );

        if self.impls.iter().find(|i| i.0 == impl_name).is_some() {
            Ok(())
        } else {
            Err(HayError::new(
                format!("Interface `{impl_name}` is not implemented"),
                token.loc.clone(),
            ))
        }
    }
}
