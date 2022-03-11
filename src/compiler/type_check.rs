use crate::compiler::compiler_error;
use crate::ir::{
    op::Op,
    types::{Signature, Type},
};

pub fn evaluate_signature(op: &Op, signature: &Signature, stack: &mut Vec<Type>) {
    if stack.len() < signature.inputs.len() {
        compiler_error(
            &op.token,
            format!("Type Error - Invalid number of inputs for `{:?}`", op.kind).as_str(),
            vec![
                format!("Expected: {:?}", signature.inputs).as_str(),
                format!("Found:    {:?}", stack).as_str(),
            ],
        )
    }

    for (input, stk) in signature.inputs.iter().rev().zip(stack.iter().rev()) {
        if input.name != stk.name {
            compiler_error(
                &op.token,
                format!("Type Error - Invalid inputs for `{:?}`", op.kind).as_str(),
                vec![
                    format!("Expected: {:?}", signature.inputs).as_str(),
                    format!(
                        "Found:    {:?}",
                        stack
                            .iter()
                            .rev()
                            .take(signature.inputs.len())
                            .rev()
                            .collect::<Vec<&Type>>()
                    )
                    .as_str(),
                ],
            )
        }
    }

    signature.inputs.iter().for_each(|_| {
        stack.pop();
    });

    signature
        .outputs
        .iter()
        .for_each(|output| stack.push(output.clone()));
}
