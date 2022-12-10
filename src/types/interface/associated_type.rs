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
            alias_list: self.annotations.clone(),
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
    pub alias_list: Vec<TypeId>,
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

    pub fn is_generic(&self, types: &TypeMap) -> bool {
        self.annotations.iter().any(|t| t.is_generic(types))
    }

    pub fn assign(
        &self,
        token: &Token,
        map: &HashMap<TypeId, TypeId>,
        types: &mut TypeMap,
    ) -> Result<TypeId, HayError> {
        let mut aliased_map = HashMap::new();
        self.alias_list
            .iter()
            .zip(self.annotations.iter())
            .for_each(|(k, v)| {
                aliased_map.insert(k.clone(), map.get(v).unwrap().clone());
            });

        let (interface_impl, idx) = match types.get(&self.interface) {
            Some(Type::InterfaceBase(req_base)) => (
                req_base.find_impl(token, &aliased_map)?,
                req_base.type_index(&self.name)?,
            ),
            _ => unreachable!(),
        };

        match types.get(&interface_impl) {
            Some(Type::InterfaceInstance(instance)) => Ok(instance
                .types
                .get(idx)
                .expect("This should be in range!")
                .clone()),
            _ => unreachable!(),
        }
    }
}
