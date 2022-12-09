use std::collections::HashMap;

use crate::{
    error::HayError,
    lex::token::Token,
    types::{Type, TypeId, TypeMap},
};

#[derive(Debug, Clone)]
pub struct AssociatedTypeBase {
    pub name: Token,
    pub interface: TypeId,
    pub annotations: Vec<TypeId>,
}

impl AssociatedTypeBase {
    pub fn assign(
        &self,
        token: &Token,
        map: &HashMap<TypeId, TypeId>,
        types: &mut TypeMap,
    ) -> Result<TypeId, HayError> {
        let mut assigned_annotations = vec![];
        for t in &self.annotations {
            assigned_annotations.push(t.assign(token, map, types)?);
        }

        let instance = AssociatedTypeInstance {
            name: self.name.clone(),
            interface: self.interface.clone(),
            annotations: assigned_annotations,
        };

        let instance_id = instance.id();
        types.insert(instance_id.clone(), Type::AssociatedTypeInstance(instance));
        Ok(instance_id)
    }
}

#[derive(Debug, Clone)]
pub struct AssociatedTypeInstance {
    pub name: Token,
    pub interface: TypeId,
    pub annotations: Vec<TypeId>,
}

impl AssociatedTypeInstance {
    pub fn id(&self) -> TypeId {
        let mut name = format!("{}<", self.interface);
        for ann in &self.annotations[0..self.annotations.len() - 1] {
            name = format!("{name}{ann} ");
        }
        name = format!(
            "{name}{}>::{}",
            self.annotations.last().unwrap(),
            self.name.lexeme
        );
        TypeId::new(name)
    }
}
