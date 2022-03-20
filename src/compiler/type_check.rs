use crate::compiler::compiler_error;
use crate::ir::{
    function::Function,
    keyword::Keyword,
    op::{Op, OpKind},
    types::{Signature, Type},
    FnTable, Frame, Stack,
};
use std::collections::HashMap;

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
        if input != stk {
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

fn check_stacks_similar(stack1: &Stack, stack2: &Stack) -> bool {
    stack1.len() == stack2.len() && stack1.iter().zip(stack2.iter()).all(|(t, f)| t == f)
}

#[allow(clippy::too_many_arguments)]
fn type_check_if_block(
    ops: &mut Vec<Op>,
    start_ip: usize,
    stack: &mut Stack,
    frame: &mut Frame,
    fn_table: &FnTable,
    type_map: &HashMap<String, Type>,
    gen_map: &HashMap<String, Type>,
    globals: &HashMap<String, (Type, String)>,
) -> (usize, Vec<Function>) {
    assert!(matches!(ops[start_ip].kind, OpKind::Nop(Keyword::If)));
    assert!(
        matches!(ops[start_ip + 1].kind, OpKind::JumpCond(Some(_))),
        "Expected JumpCond, but found {:?}",
        ops[start_ip + 1].kind
    );

    ops[start_ip + 1].type_check(stack, frame, fn_table, type_map, gen_map, globals);

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
        type_map,
        gen_map,
        globals,
        vec![Box::new(move |op| match op.kind {
            OpKind::JumpDest(n) => n >= jump_dest,
            _ => false,
        })],
    );

    new_fns.append(&mut new_fns_if_true);
    let mut false_start_ip = jump_dest;
    if jump_dest + 1 < ops.len() && matches!(ops[jump_dest + 1].kind, OpKind::Nop(Keyword::Else)) {
        false_start_ip += 1;
    }

    let mut stack_if_false = stack.clone();
    let mut frame_if_false = frame.clone();
    let (false_end_ip, mut new_fns_if_false) = type_check_ops_list(
        ops,
        false_start_ip,
        &mut stack_if_false,
        &mut frame_if_false,
        fn_table,
        type_map,
        gen_map,
        globals,
        vec![Box::new(move |op| match op.kind {
            OpKind::JumpDest(n) => n >= jump_dest,
            _ => false,
        })],
    );

    new_fns.append(&mut new_fns_if_false);

    if !check_stacks_similar(&stack_if_true, &stack_if_false) {
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

#[allow(clippy::too_many_arguments)]
pub fn type_check_while_block(
    ops: &mut Vec<Op>,
    start_ip: usize,
    stack: &mut Stack,
    frame: &mut Frame,
    fn_table: &FnTable,
    type_map: &HashMap<String, Type>,
    gen_map: &HashMap<String, Type>,
    globals: &HashMap<String, (Type, String)>,
) -> (usize, Vec<Function>) {
    let initial_stack = stack.clone();
    let initial_frame = frame.clone();
    let (jump_cond_ip, mut pre_jump_fns) = type_check_ops_list(
        ops,
        start_ip + 1,
        stack,
        frame,
        fn_table,
        type_map,
        gen_map,
        globals,
        vec![Box::new(|op| matches!(op.kind, OpKind::JumpCond(Some(_))))],
    );

    ops[jump_cond_ip].type_check(stack, frame, fn_table, type_map, gen_map, globals);
    assert!(matches!(ops[jump_cond_ip].kind, OpKind::JumpCond(Some(_))));
    let jump_cond_dest = match ops[jump_cond_ip].kind {
        OpKind::JumpCond(Some(n)) => n,
        _ => unreachable!(),
    };

    let mut while_body_stack = initial_stack.clone();
    let mut while_body_frame = initial_frame.clone();
    let (jump_ip, mut while_body_fns) = type_check_ops_list(
        ops,
        jump_cond_ip + 1,
        &mut while_body_stack,
        &mut while_body_frame,
        fn_table,
        type_map,
        gen_map,
        globals,
        vec![Box::new(move |op| matches!(op.kind, OpKind::Jump(Some(_))))],
    );
    assert!(matches!(ops[jump_ip].kind, OpKind::Jump(Some(_))));
    let jump_dest = match ops[jump_ip].kind {
        OpKind::Jump(Some(n)) => n,
        _ => unreachable!(),
    };

    assert_eq!(jump_dest, start_ip + 1);

    if !check_stacks_similar(&while_body_stack, &initial_stack) {
        compiler_error(
            &ops[jump_cond_ip].token,
            "Branches do not create similar stacks",
            vec![
                format!("Stack Before while body:  {:?}", initial_stack).as_str(),
                format!("Stack After while body:   {:?}", while_body_stack).as_str(),
            ],
        )
    }
    assert_eq!(initial_frame, while_body_frame);
    while_body_fns.append(&mut pre_jump_fns);
    (jump_cond_dest, while_body_fns)
}

#[allow(clippy::too_many_arguments)]
pub fn type_check_ops_list(
    ops: &mut Vec<Op>,
    start_ip: usize,
    stack: &mut Stack,
    frame: &mut Frame,
    fn_table: &FnTable,
    type_map: &HashMap<String, Type>,
    gen_map: &HashMap<String, Type>,
    globals: &HashMap<String, (Type, String)>,
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
                let (end_ip, mut if_new_fns) = type_check_if_block(
                    ops, ip, stack, frame, fn_table, type_map, gen_map, globals,
                );
                ip = end_ip;
                new_fns.append(&mut if_new_fns);
            }
            OpKind::Nop(Keyword::While) => {
                let (end_ip, mut whlie_new_fns) = type_check_while_block(
                    ops, ip, stack, frame, fn_table, type_map, gen_map, globals,
                );
                ip = end_ip;
                new_fns.append(&mut whlie_new_fns);
            }
            OpKind::Jump(Some(n)) => {
                ip = n;
            }
            _ => {
                if let Some(f) =
                    ops[ip].type_check(stack, frame, fn_table, type_map, gen_map, globals)
                {
                    new_fns.push(f);
                }
                ip += 1;
            }
        }
    }

    (ip, new_fns)
}
