use crate::compiler::compiler_error;
use crate::ir::{
    function::Function,
    keyword::Keyword,
    op::{Op, OpKind},
    types::{Signature, Type},
    FnTable, Frame, Stack,
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

fn type_check_if_block(
    ops: &mut Vec<Op>,
    start_ip: usize,
    stack: &mut Stack,
    frame: &mut Frame,
    fn_table: &FnTable,
) -> (usize, Vec<Function>) {
    assert!(matches!(ops[start_ip].kind, OpKind::Nop(Keyword::If)));
    assert!(
        matches!(ops[start_ip + 1].kind, OpKind::JumpCond(Some(_))),
        "Expected JumpCond, but found {:?}",
        ops[start_ip + 1].kind
    );

    ops[start_ip + 1].type_check(stack, frame, fn_table);

    let jump_dest = match ops[start_ip + 1].kind {
        OpKind::JumpCond(Some(n)) => n,
        _ => unreachable!(),
    };
    let mut new_fns = vec![];

    let mut stack_if_true = stack.clone();
    let mut frame_if_true = frame.clone();
    let (true_end_ip, mut new_fns_if_true) = type_check_ops_list(
        ops,
        start_ip + 2,
        &mut stack_if_true,
        &mut frame_if_true,
        fn_table,
        vec![Box::new(|op| matches!(op.kind, OpKind::JumpDest(_)))],
    );
    new_fns.append(&mut new_fns_if_true);

    let mut stack_if_false = stack.clone();
    let mut frame_if_false = frame.clone();
    let (false_end_ip, mut new_fns_if_false) = type_check_ops_list(
        ops,
        jump_dest + 1,
        &mut stack_if_false,
        &mut frame_if_false,
        fn_table,
        vec![Box::new(|op| matches!(op.kind, OpKind::JumpDest(_)))],
    );

    new_fns.append(&mut new_fns_if_false);

    if stack_if_true != stack_if_false {
        compiler_error(
            &ops[start_ip].token,
            "Branches do not create similar stacks",
            vec![
                format!("Stack if True:  {:?}", stack_if_true).as_str(),
                format!("Stack if False: {:?}", stack_if_false).as_str(),
            ],
        )
    }

    assert_eq!(frame_if_true, frame_if_false);
    assert_eq!(true_end_ip, false_end_ip);
    *stack = stack_if_true;
    *frame = frame_if_true;

    (true_end_ip, new_fns)
}

pub fn type_check_ops_list(
    ops: &mut Vec<Op>,
    start_ip: usize,
    stack: &mut Stack,
    frame: &mut Frame,
    fn_table: &FnTable,
    break_on: Vec<Box<dyn Fn(&Op) -> bool>>,
) -> (usize, Vec<Function>) {
    let mut ip = start_ip;
    let mut new_fns = vec![];
    while ip < ops.len() {
        if break_on.iter().any(|f| f(&ops[ip])) {
            return (ip, new_fns);
        }
        match ops[ip].kind {
            OpKind::Nop(Keyword::If) => {
                let (end_ip, mut if_new_fns) = type_check_if_block(ops, ip, stack, frame, fn_table);
                ip = end_ip;
                new_fns.append(&mut if_new_fns);
            }
            OpKind::Jump(Some(n)) => {
                ip = n;
            }
            _ => {
                if let Some(f) = ops[ip].type_check(stack, frame, fn_table) {
                    new_fns.push(f);
                }
                ip += 1;
            }
        }
    }

    (ip, new_fns)
}
