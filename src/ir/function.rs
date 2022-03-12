use crate::compiler::{compiler_error, type_check_ops_list};
use crate::ir::{
    op::Op,
    token::Token,
    types::{Signature, Type},
    FnTable, Stack,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub token: Token,
    pub gen: Vec<Type>,
    pub sig: Signature,
    pub ops: Vec<Op>,
}

impl Function {
    pub fn is_generic(&self) -> bool {
        !self.gen.is_empty()
    }

    pub fn type_check(&mut self, fn_table: &FnTable) -> Option<Vec<Function>> {
        if self.is_generic() {
            return None;
        }
        let (mut stack, mut frame) = self.sig.create_stack_and_frame();
        let (_, new_fns) =
            type_check_ops_list(&mut self.ops, 0, &mut stack, &mut frame, fn_table, vec![]);
        self.check_output(&stack);

        if new_fns.is_empty() {
            None
        } else {
            Some(new_fns)
        }
    }

    fn check_output(&self, stack: &Stack) {
        if self.sig.outputs.len() != stack.len() {
            compiler_error(
                &self.token,
                format!(
                    "Type Error - Function `{}` doesn't produce the correct number of outputs",
                    self.name
                )
                .as_str(),
                vec![
                    format!("Expected: {:?}", self.sig.outputs).as_str(),
                    format!("Found:    {:?}", stack).as_str(),
                ],
            )
        }

        for (output, stk) in self.sig.outputs.iter().rev().zip(stack.iter().rev()) {
            if output.name != stk.name {
                compiler_error(
                    &self.token,
                    format!(
                        "Type Error - Function `{}` doesn't produce the correct outputs",
                        self.name
                    )
                    .as_str(),
                    vec![
                        format!("Expected: {:?}", self.sig.outputs).as_str(),
                        format!("Found:    {:?}", stack).as_str(),
                    ],
                )
            }
        }
    }

    pub fn make_concrete(&self, stack: &Stack) -> Self {
        let mut generic_map: HashMap<String, Option<Type>> = HashMap::new();
        self.gen.iter().for_each(|t| {
            if generic_map.insert(t.name.clone(), None).is_some() {
                compiler_error(
                    &self.token,
                    "Cannot use the same identifier multiple times in function generic list",
                    vec![format!("Type `{}` is used multiple times", t.name).as_str()],
                )
            }
        });

        let mut sig = Signature {
            inputs: vec![],
            outputs: vec![],
        };
        let mut name = self.name.clone();
        name.push('<');

        self.sig
            .inputs
            .iter()
            .rev()
            .zip(stack.iter().rev())
            .for_each(|(t, s)| {
                if generic_map.contains_key(&t.name) {
                    if let Some(Some(old)) = generic_map.insert(t.name.clone(), Some(s.clone())) {
                        if old.name != s.name {
                            compiler_error(
                                &self.token,
                                format!("Generic Type Resolution Failure for `{}", self.name)
                                    .as_str(),
                                vec![
                                    format!(
                                        "Generic type `{}` was assigned to both `{}` and `{}`",
                                        t, s, old
                                    )
                                    .as_str(),
                                    format!("Expected: {:?}", self.sig.inputs).as_str(),
                                    format!(
                                        "Found:    {:?}",
                                        stack
                                            .iter()
                                            .rev()
                                            .take(self.sig.inputs.len())
                                            .rev()
                                            .collect::<Vec<&Type>>()
                                    )
                                    .as_str(),
                                ],
                            );
                        }
                    } else {
                        name.push_str(format!("{}={}", t.name, s).as_str());
                        sig.inputs.push(Type {
                            name: s.name.clone(),
                            ident: t.ident.clone(),
                        });
                    }
                }
            });
        name.push('>');
        self.sig.outputs.iter().for_each(|t| {
            if let Some(maybe_t) = generic_map.get(&t.name) {
                if let Some(typ) = maybe_t {
                    sig.outputs.push(typ.clone());
                } else {
                    compiler_error(
                        &self.token,
                        format!("Type `{}` was never assigned.", t).as_str(),
                        vec![],
                    );
                }
            }
        });

        sig.inputs.reverse();

        Function {
            name,
            token: self.token.clone(),
            gen: vec![],
            sig,
            ops: self.ops.clone(),
        }
    }
}
