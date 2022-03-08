use crate::ir::{Function, Op, Program, Signature, Type};

fn evaluate_signature(signature: Signature, stack: &mut Vec<Type>) {
    let tail: Vec<&Type> = stack.iter().rev().take(signature.inputs.len()).collect();
    signature.inputs.iter().zip(tail).for_each(|(input, typ)| {
        if input.name != typ.name {
            panic!("Compiler error isn't implemented yet for type checking...");
            // compiler_error()
        }
    });

    signature
        .outputs
        .iter()
        .for_each(|output| stack.push(output.clone()));
}

fn type_check_op(op: &Op, stack: &mut Vec<Type>, frame: &mut Vec<Type>, _program: &Program) {
    match op {
        Op::PushIdent(n) => evaluate_signature(
            Signature {
                inputs: vec![],
                outputs: vec![frame[*n].clone()],
            },
            stack,
        ),
        Op::PushInt(_) => evaluate_signature(
            Signature {
                inputs: vec![],
                outputs: vec![Type::u64_t()],
            },
            stack,
        ),
        Op::PushBool(_) => evaluate_signature(
            Signature {
                inputs: vec![],
                outputs: vec![Type::bool_t()],
            },
            stack,
        ),
        Op::MakeIdent(_) => {
            if let Some(typ) = stack.pop() {
                frame.push(typ);
            } else {
                panic!("Compiler Errors aren't supported for type checking yet...");
            }
        }
        Op::Print => evaluate_signature(
            Signature {
                inputs: vec![Type::u64_t()],
                outputs: vec![],
            },
            stack,
        ),
        o => println!("Type Checking {:?} isn't implemented yet...", o),
    }
}

fn type_check_function(func: &Function, program: &Program) {
    let mut stack: Vec<Type> = vec![];
    let mut frame: Vec<Type> = vec![];

    if func.sig.inputs.iter().all(|input| input.ident.is_some()) {
        func.sig
            .inputs
            .iter()
            .rev()
            .for_each(|input| frame.push(input.clone()));
    } else {
        func.sig
            .inputs
            .iter()
            .for_each(|input| stack.push(input.clone()));
    }

    func.ops
        .iter()
        .for_each(|op| type_check_op(op, &mut stack, &mut frame, program));
}

pub fn type_check_program(program: &Program) {
    program
        .functions
        .iter()
        .for_each(|func| type_check_function(&func, &program))
}
