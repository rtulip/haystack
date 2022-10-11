#![allow(dead_code)]
use crate::{ast::stmt::Stmt, error::HayError, types::Type};
use std::collections::HashMap;

pub struct Environment {
    scopes: Vec<HashMap<String, Type>>,
    stack: Vec<Type>,
}

impl Environment {
    pub fn new(mut stmts: Vec<Stmt>) -> Result<Self, HayError> {
        let mut scope = HashMap::new();
        while let Some(s) = stmts.pop() {
            let (name, typ) = s.into_type();
            scope.insert(name, typ);
        }
        Ok(Environment {
            scopes: vec![scope],
            stack: vec![],
        })
    }

    pub fn find(&self, name: &String) -> Option<&Type> {
        for i in (0..self.scopes.len()).into_iter().rev() {
            if self.scopes[i].contains_key(name) {
                return self.scopes[i].get(name);
            }
        }

        None
    }
}
