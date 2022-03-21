use crate::compiler::{compiler_error, type_check_ops_list};
use crate::ir::{
    data::InitData,
    op::Op,
    token::Token,
    types::{Signature, Type},
    FnTable, Stack,
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct LocalVar {
    pub typ: Type,
    pub size: u64,
    pub value: Option<InitData>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub token: Token,
    pub gen: Vec<Type>,
    pub sig: Signature,
    pub ops: Vec<Op>,
    pub gen_map: HashMap<String, Type>,
    pub locals: BTreeMap<String, LocalVar>,
}

impl Function {
    pub fn is_generic(&self) -> bool {
        !self.gen.is_empty()
    }

    pub fn locals_offset(locals: &BTreeMap<String, LocalVar>) -> usize {
        locals.iter().map(|(_, local)| local.size as usize).sum()
    }

    pub fn locals_get_offset(ident: &String, locals: &BTreeMap<String, LocalVar>) -> (Type, usize) {
        let mut size = 0;
        for (loc_name, local) in locals {
            size += local.size;
            if ident == loc_name {
                return (local.typ.clone(), size as usize);
            }
        }
        panic!("didn't find {ident} in locals");
    }

    pub fn type_check(
        &mut self,
        fn_table: &FnTable,
        type_map: &HashMap<String, Type>,
        globals: &HashMap<String, (Type, String)>,
    ) -> Option<Vec<Function>> {
        if self.is_generic() {
            return None;
        }

        let mut stack = self.sig.inputs.clone();
        let mut frame = vec![];
        let (_, new_fns) = type_check_ops_list(
            &mut self.ops,
            0,
            &mut stack,
            &mut frame,
            fn_table,
            type_map,
            &self.gen_map,
            &self.locals,
            globals,
            vec![],
        );
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
            if output != stk {
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

    pub fn resolve_generic_function(&self, token: &Token, stack: &Stack) -> Self {
        if stack.len() < self.sig.inputs.len() {
            compiler_error(
                token,
                format!("Insufficient arguments for {}", self.name).as_str(),
                vec![
                    format!("Expected: {:?}", self.sig.inputs).as_str(),
                    format!("Found:    {:?}", stack).as_str(),
                ],
            )
        }

        let pairs: Vec<(&Type, &Type)> = self
            .sig
            .inputs
            .iter()
            .zip(stack[stack.len() - self.sig.inputs.len()..].iter())
            .collect();
        let mut map: HashMap<String, Type> = HashMap::new();
        let resolved_inputs = pairs
            .iter()
            .map(|(t1, t2)| Type::resolve_type(token, t1, t2, &mut map, &HashMap::new()))
            .collect::<Vec<Type>>();

        if !self
            .gen
            .iter()
            .all(|t| map.contains_key(&format!("{:?}", t)))
        {
            compiler_error(
                token,
                "Some types were not resolved during cast",
                vec![format!(
                    "These types were not resolved: {:?}",
                    self.gen
                        .iter()
                        .filter(|t| !map.contains_key(&format!("{:?}", t)))
                        .collect::<Vec<&Type>>()
                )
                .as_str()],
            )
        }

        let resolved_outputs = self
            .sig
            .outputs
            .iter()
            .map(|t| Type::assign_generics(token, t, &map))
            .collect::<Vec<Type>>();

        let sig = Signature {
            inputs: resolved_inputs,
            outputs: resolved_outputs,
        };

        let mut new_name = self.name.clone();
        new_name.push('<');

        let assignments_strs = map
            .iter()
            .map(|(k, v)| format!("{}={:?}", k, v))
            .collect::<Vec<String>>();

        new_name.push_str(assignments_strs[0].as_str());
        for s in assignments_strs[1..].iter() {
            new_name.push(' ');
            new_name.push_str(s.as_str());
        }
        new_name.push('>');

        let new_ops = self.ops.clone();
        Function {
            name: new_name,
            token: self.token.clone(),
            gen: vec![],
            sig,
            ops: new_ops,
            gen_map: map,
            locals: self.locals.clone(),
        }
    }
}
