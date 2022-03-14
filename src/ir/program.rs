use crate::compiler::compiler_error;
use crate::ir::{function::Function, op::OpKind, token::Token, types::Type};

use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Serialize, Deserialize)]
pub struct Program {
    pub types: HashMap<String, Type>,
    pub functions: Vec<Function>,
    pub strings: Vec<String>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            types: HashMap::from_iter([
                (String::from("u64"), Type::U64),
                (String::from("bool"), Type::Bool),
                (String::from("ptr"), Type::Ptr),
                (String::from("Str"), Type::str()),
            ]),
            functions: vec![],
            strings: vec![],
        }
    }

    pub fn check_for_entry_point(&self) {
        if !self.functions.iter().any(|f| f.name == "main") {
            let token = if !self.functions.is_empty() {
                self.functions.last().unwrap().token.clone()
            } else {
                Token::default()
            };
            compiler_error(
                &token,
                "No entry point defined",
                vec!["Try adding a `main` function."],
            );
        }
    }
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
                    if let Some(mut fns) = f.type_check(&fn_table, &self.types) {
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

    pub fn check_for_name_conflicts(&self) {
        let mut name_map: HashMap<&String, (NameKind, &Token)> = HashMap::new();

        self.functions.iter().for_each(|f| {
            if let Some((kind, tok)) = name_map.insert(&f.name, (NameKind::Function, &f.token)) {
                compiler_error(
                    &f.token,
                    format!("Redefinition of {} `{}`", kind, f.name).as_str(),
                    vec![format!("Originally defined here: {}", tok.loc).as_str()],
                )
            }

            let mut vars: Vec<&String> = vec![];
            f.ops.iter().for_each(|op| match &op.kind {
                OpKind::MakeIdent { ident: s, .. } => {
                    vars.push(s);
                    if let Some((kind, tok)) = name_map.insert(s, (NameKind::Var, &op.token)) {
                        compiler_error(
                            &op.token,
                            format!("Redefinition of {} `{}`", kind, s).as_str(),
                            vec![format!("Originally defined here: {}", tok.loc).as_str()],
                        )
                    }
                }
                OpKind::EndBlock(n) => {
                    for _ in 0..*n {
                        let s = vars.pop().unwrap();
                        name_map.remove(&s).unwrap();
                    }
                }
                OpKind::Return => vars.drain(..).for_each(|s| {
                    name_map.remove(&s).unwrap();
                }),
                _ => (),
            });
        })
    }

    pub fn meta(&self) -> HashMap<String, Function> {
        let mut fn_names: HashMap<String, Function> = HashMap::new();
        self.functions.iter().for_each(|func| {
            fn_names.insert(func.name.clone(), func.clone());
        });
        fn_names
    }

    pub fn assign_words(&mut self) {
        let fn_names = self.meta();
        self.functions.iter_mut().for_each(|func| {
            let mut scope: Vec<String> = vec![];
            if func
                .sig_idents
                .iter()
                .all(|maybe_ident| maybe_ident.is_some())
            {
                func.sig_idents.iter().rev().for_each(|input| {
                    if let Some(ident) = input {
                        scope.push(ident.clone())
                    }
                })
            } else if !func
                .sig_idents
                .iter()
                .all(|maybe_ident| maybe_ident.is_none())
            {
                compiler_error(&func.token, "All inputs must be named if any are.", vec![]);
            }
            for op in &mut func.ops {
                match &mut op.kind {
                    OpKind::MakeIdent { ident: s, .. } => {
                        scope.push(s.clone());
                    }
                    OpKind::EndBlock(n) => {
                        for _ in 0..*n {
                            scope.pop();
                        }
                    }
                    OpKind::Word(s) => {
                        if let Some(idx) = &scope.iter().position(|ident| ident == s) {
                            op.kind = OpKind::PushIdent {
                                index: *idx,
                                offset: None,
                                size: None,
                            };
                        } else if fn_names.get(s).is_some() {
                            op.kind = OpKind::Call(s.clone());
                        } else {
                            compiler_error(
                                &op.token,
                                format!("Unrecognized Word: `{s}`").as_str(),
                                vec![],
                            )
                        }
                    }
                    _ => (),
                }
            }
        });
    }
}

enum NameKind {
    Var,
    Function,
}

impl std::fmt::Display for NameKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            NameKind::Var => write!(f, "Var"),
            NameKind::Function => write!(f, "Function"),
        }
    }
}
