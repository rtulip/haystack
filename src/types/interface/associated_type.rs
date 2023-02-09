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

        if assigned_annotations.iter().all(|t| !t.is_generic(types)) {
            let mut name = format!("{}<", self.interface);
            for t in &assigned_annotations[0..assigned_annotations.len() - 1] {
                name = format!("{name}{t} ");
            }
            name = format!("{name}{}>", assigned_annotations.last().unwrap());

            let concrete_tid = TypeId::new(name);

            match (types.get(&self.interface), types.get(&concrete_tid)) {
                (
                    Some(Type::InterfaceBase(interface_base)),
                    Some(Type::InterfaceInstance(ref interface_instance)),
                ) => {
                    let idx = interface_base.type_index(&self.name)?;
                    return Ok(interface_instance.types[idx].clone());
                }
                (Some(Type::InterfaceBase(_)), None) => {
                    return Err(HayError::new(
                        format!(
                            "Failed to find associated type {} for {concrete_tid}",
                            self.name.lexeme
                        ),
                        token.loc.clone(),
                    )
                    .with_hint(format!(
                        "Interface {} is not implemented for {concrete_tid}",
                        self.interface
                    )))
                }
                _ => unimplemented!(),
            }
        }

        let instance = AssociatedTypeInstance {
            name: self.name.clone(),
            interface: self.interface.clone(),
            alias_list: self.annotations.clone(),
            annotations: assigned_annotations.clone(),
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
