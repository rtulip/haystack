use crate::compiler::compiler_error;
use crate::ir::{Function, Op, OpKind, Program, Signature, Type};

fn evaluate_signature(op: &Op, signature: Signature, stack: &mut Vec<Type>) {
    let tail: Vec<&Type> = stack.iter().rev().take(signature.inputs.len()).collect();
    signature.inputs.iter().zip(&tail).for_each(|(input, typ)| {
        if input.name != typ.name {
            compiler_error(
                &op.token,
                format!("Type Error -- Invalid inputs for `{:?}`", op.kind).as_str(),
                vec![
                    format!("Expected: {:?}", signature.inputs).as_str(),
                    format!("Found:    {:?}", tail).as_str(),
                ],
            )
        }
    });

    signature
        .outputs
        .iter()
        .for_each(|output| stack.push(output.clone()));
}

fn type_check_op(op: &Op, stack: &mut Vec<Type>, frame: &mut Vec<Type>, _program: &Program) {
    match &op.kind {
        OpKind::Add => evaluate_signature(
            op,
            Signature {
                inputs: vec![Type::u64_t(), Type::u64_t()],
                outputs: vec![Type::u64_t()],
            },
            stack,
        ),
        OpKind::Sub => evaluate_signature(
            op,
            Signature {
                inputs: vec![Type::u64_t(), Type::u64_t()],
                outputs: vec![Type::u64_t()],
            },
            stack,
        ),
        OpKind::Mul => evaluate_signature(
            op,
            Signature {
                inputs: vec![Type::u64_t(), Type::u64_t()],
                outputs: vec![Type::u64_t()],
            },
            stack,
        ),
        OpKind::Div => evaluate_signature(
            op,
            Signature {
                inputs: vec![Type::u64_t(), Type::u64_t()],
                outputs: vec![Type::u64_t()],
            },
            stack,
        ),
        OpKind::LessThan => evaluate_signature(
            op,
            Signature {
                inputs: vec![Type::u64_t(), Type::u64_t()],
                outputs: vec![Type::bool_t()],
            },
            stack,
        ),
        OpKind::LessEqual => evaluate_signature(
            op,
            Signature {
                inputs: vec![Type::u64_t(), Type::u64_t()],
                outputs: vec![Type::bool_t()],
            },
            stack,
        ),
        OpKind::GreaterThan => evaluate_signature(
            op,
            Signature {
                inputs: vec![Type::u64_t(), Type::u64_t()],
                outputs: vec![Type::bool_t()],
            },
            stack,
        ),
        OpKind::GreaterEqual => evaluate_signature(
            op,
            Signature {
                inputs: vec![Type::u64_t(), Type::u64_t()],
                outputs: vec![Type::bool_t()],
            },
            stack,
        ),
        OpKind::Equals => evaluate_signature(
            op,
            Signature {
                inputs: vec![Type::u64_t(), Type::u64_t()],
                outputs: vec![Type::bool_t()],
            },
            stack,
        ),
        OpKind::NotEquals => evaluate_signature(
            op,
            Signature {
                inputs: vec![Type::u64_t(), Type::u64_t()],
                outputs: vec![Type::bool_t()],
            },
            stack,
        ),
        OpKind::PushIdent(n) => evaluate_signature(
            op,
            Signature {
                inputs: vec![],
                outputs: vec![frame[*n].clone()],
            },
            stack,
        ),
        OpKind::PushString(_) => todo!(),
        OpKind::PushInt(_) => evaluate_signature(
            op,
            Signature {
                inputs: vec![],
                outputs: vec![Type::u64_t()],
            },
            stack,
        ),
        OpKind::PushBool(_) => evaluate_signature(
            op,
            Signature {
                inputs: vec![],
                outputs: vec![Type::bool_t()],
            },
            stack,
        ),
        OpKind::MakeIdent(_) => {
            if let Some(typ) = stack.pop() {
                frame.push(typ);
            } else {
                compiler_error(
                    &op.token,
                    "Type Error - Creating a `var` requires at least one element on the stack.",
                    vec![format!("Stack: {:?}", stack).as_str()],
                )
            }
        }
        OpKind::Print => evaluate_signature(
            op,
            Signature {
                inputs: vec![Type::u64_t()],
                outputs: vec![],
            },
            stack,
        ),
        OpKind::Return => *frame = Vec::<Type>::new(),
        OpKind::Call(_func) => todo!("Type Checking calls isn't implemented yet"),
        OpKind::Word(_) => unreachable!("Shouldn't have any words left to type check"),
        OpKind::PrepareFunc(_) => unreachable!("PrepareFunction shouldn't be type checked."),
        OpKind::Default => unreachable!("Default op shouldn't be compiled"),
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
    program.functions.iter().for_each(|func| {
        if func.gen.len() == 0 {
            type_check_function(&func, &program)
        }
    })
}
