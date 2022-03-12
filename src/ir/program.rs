use crate::ir::{function::Function, op::OpKind};

use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Program {
    pub functions: Vec<Function>,
}

impl Program {
    pub fn type_check(&mut self) {
        let mut fn_table: HashMap<String, Function> = HashMap::new();
        let mut checked: HashSet<String> = HashSet::new();
        self.functions.iter().for_each(|f| {
            fn_table.insert(f.name.clone(), f.clone());
        });
        let mut new_fns: Vec<Function> = vec![];

        loop {
            self.functions.iter_mut().for_each(|f| {
                if !checked.contains(&f.name) {
                    if let Some(mut fns) = f.type_check(&fn_table) {
                        new_fns.append(&mut fns);
                    }
                    checked.insert(f.name.clone());
                }
            });

            if new_fns.is_empty() {
                break;
            } else {
                new_fns.drain(..).for_each(|f| {
                    if fn_table.insert(f.name.clone(), f.clone()).is_none() {
                        self.functions.push(f);
                    }
                });
            }
        }
    }

    pub fn normalize_function_names(&mut self) {
        let mut fn_name_map: HashMap<String, String> = HashMap::new();
        self.functions.iter().enumerate().for_each(|(i, f)| {
            let new_name = match f.name.as_str() {
                "main" => String::from("main"),
                _ => format!("fn_{}", i),
            };
            fn_name_map.insert(f.name.clone(), new_name);
        });

        self.functions.iter_mut().for_each(|f| {
            f.name = fn_name_map.get(&f.name).unwrap().clone();
            f.ops.iter_mut().for_each(|op| {
                if let Some(fn_name) = match &op.kind {
                    OpKind::Call(fn_name) => Some(fn_name),
                    _ => None,
                } {
                    op.kind = OpKind::Call(fn_name_map.get(fn_name).unwrap().clone());
                }
            })
        });
    }
}
