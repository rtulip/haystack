use crate::ir::{Op, Program};
use std::collections::HashSet;

pub fn assign_words(program: &mut Program) {
    let mut fn_names: HashSet<String> = HashSet::new();

    for func in &program.functions {
        if !fn_names.insert(func.name.clone()) {
            panic!("Compiler Error: Redefinition of `{}`", func.name)
        }
    }

    for func in &mut program.functions {
        let mut scope: Vec<String> = vec![];
        if func.sig.inputs.iter().all(|typ| typ.ident.is_some()) {
            for input in &func.sig.inputs {
                if let Some(ident) = &input.ident {
                    scope.push(ident.clone())
                }
            }
        } else if !func.sig.inputs.iter().all(|typ| typ.ident.is_none()) {
            panic!("Compiler Error: All inputs must be named if any are.")
        }

        for op in &mut func.ops {
            match op {
                Op::Word(s) => {
                    if let Some(idx) = &scope.iter().position(|ident| ident == s) {
                        *op = Op::PushIdent(*idx);
                    } else if fn_names.contains(s) {
                        *op = Op::Call(s.clone());
                    }
                }
                _ => (),
            }
        }
    }
}
