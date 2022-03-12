use crate::compiler::program_meta;
use crate::ir::{op::OpKind, Program};

pub fn assign_words(program: &mut Program) {
    let fn_names = program_meta(program);

    for func in &mut program.functions {
        let mut scope: Vec<String> = vec![];
        if func.sig.inputs.iter().all(|typ| typ.ident.is_some()) {
            func.sig.inputs.iter().rev().for_each(|input| {
                if let Some(ident) = &input.ident {
                    scope.push(ident.clone())
                }
            })
        } else if !func.sig.inputs.iter().all(|typ| typ.ident.is_none()) {
            panic!("Compiler Error: All inputs must be named if any are.")
        }

        for op in &mut func.ops {
            match &mut op.kind {
                OpKind::MakeIdent(s) => {
                    scope.push(s.clone());
                }
                OpKind::EndBlock(n) => {
                    for _ in 0..*n {
                        scope.pop();
                    }
                }
                OpKind::Word(s) => {
                    if let Some(idx) = &scope.iter().position(|ident| ident == s) {
                        op.kind = OpKind::PushIdent(*idx);
                    } else if fn_names.get(s).is_some() {
                        op.kind = OpKind::Call(s.clone());
                    } else {
                        panic!("Unrecognized word: {s}");
                    }
                }
                _ => (),
            }
        }
    }
}
