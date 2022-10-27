use std::collections::{BTreeMap, HashMap};

use crate::{
    ast::{arg::Arg, expr::TypedExpr},
    lex::token::{Literal, Operator},
    types::{RecordKind, Type, TypeId},
};

#[derive(Debug, Clone)]
pub enum Data {
    String(String),
}

#[derive(Debug, Clone)]
pub enum Operation {
    Call(String),
    PushU64(u64),
    PushGlobal { id: String },
    PushFromFrame { offset: usize, bytes: usize },
    FromStackToFrame { bytes: usize },
    PopFramed { bytes: usize },
    Operator { op: Operator },
    JumpDest { id: usize },
    JumpFalse { dest_id: usize },
    Jump { dest_id: usize },
    Syscall(usize),
    StartBlock,
    EndBlock,
    FrameReserve { bytes: usize },
}

impl Operation {
    fn from_expr(
        expr: TypedExpr,
        types: &BTreeMap<TypeId, Type>,
        data: &mut HashMap<usize, Data>,
        jump_count: &mut usize,
        frame_reserved: &mut usize,
    ) -> Vec<Self> {
        let mut ops = vec![];

        match expr {
            TypedExpr::Framed { frame, idx, inner } => {
                assert!(idx < frame.len());
                let mut offset = 0;
                for (_, tid) in &frame[0..idx] {
                    offset += tid.size(types).unwrap();
                }

                let mut typ = &frame[idx].1;
                let bytes = if let Some(inner) = inner {
                    for inner in inner {
                        typ = if let Type::Record { members, kind, .. } = types.get(&typ).unwrap() {
                            match kind {
                                RecordKind::Struct => {
                                    let idx = members
                                        .iter()
                                        .enumerate()
                                        .find(|(_, m)| m.ident.lexeme == inner)
                                        .unwrap()
                                        .0;

                                    members
                                        .iter()
                                        .rev()
                                        .take(members.len() - idx - 1)
                                        .for_each(|m| offset += m.typ.0.size(types).unwrap());

                                    &members[idx].typ.0
                                }
                                RecordKind::Union => todo!(),
                            }
                        } else {
                            panic!("{typ}");
                        }
                    }

                    typ.size(types).unwrap()
                } else {
                    typ.size(types).unwrap()
                };

                ops.push(Operation::PushFromFrame { offset, bytes })
            }
            TypedExpr::Operator { op } => ops.push(Operation::Operator { op }),
            TypedExpr::If {
                then,
                otherwise,
                finally,
            } => {
                let mut n_jumps = 0;
                let mut then_ops = vec![];
                for e in then {
                    then_ops.append(&mut Operation::from_expr(
                        e,
                        types,
                        data,
                        jump_count,
                        frame_reserved,
                    ));
                }

                let mut otherwise_ops = vec![];
                for other in otherwise {
                    if let TypedExpr::ElseIf { condition, block } = other {
                        let mut cnd_ops = vec![];
                        let mut blk_ops = vec![];

                        for e in condition {
                            cnd_ops.append(&mut Operation::from_expr(
                                e,
                                types,
                                data,
                                jump_count,
                                frame_reserved,
                            ));
                        }

                        for e in block {
                            blk_ops.append(&mut Operation::from_expr(
                                e,
                                types,
                                data,
                                jump_count,
                                frame_reserved,
                            ));
                        }

                        otherwise_ops.push((cnd_ops, blk_ops))
                    } else {
                        panic!("{:?}", other);
                    }
                    n_jumps += 1;
                }

                let mut finally_ops = vec![];
                if let Some(finally) = finally {
                    for e in finally {
                        finally_ops.append(&mut Operation::from_expr(
                            e,
                            types,
                            data,
                            jump_count,
                            frame_reserved,
                        ));
                    }
                    n_jumps += 1;
                }

                ops.push(Operation::JumpFalse {
                    dest_id: *jump_count,
                });
                ops.append(&mut then_ops);
                ops.push(Operation::Jump {
                    dest_id: *jump_count + n_jumps,
                });
                ops.push(Operation::JumpDest { id: *jump_count });
                *jump_count += 1;

                for (mut cond, mut block) in otherwise_ops {
                    n_jumps -= 1;

                    ops.append(&mut cond);
                    ops.push(Operation::JumpFalse {
                        dest_id: *jump_count,
                    });
                    ops.append(&mut block);
                    ops.push(Operation::Jump {
                        dest_id: *jump_count + n_jumps,
                    });
                    ops.push(Operation::JumpDest { id: *jump_count });
                    *jump_count += 1;
                }

                if !finally_ops.is_empty() {
                    n_jumps -= 1;
                    ops.append(&mut finally_ops);
                    ops.push(Operation::Jump {
                        dest_id: *jump_count + n_jumps,
                    });
                    ops.push(Operation::JumpDest { id: *jump_count });
                    *jump_count += 1;
                }
            }
            TypedExpr::Literal { value } => match value {
                Literal::String(s) => {
                    let n = data.len();
                    let str_len = s.len() as u64;
                    data.insert(n, Data::String(s));
                    ops.push(Operation::PushU64(str_len));
                    ops.push(Operation::PushGlobal {
                        id: format!("str_{n}"),
                    });
                }
                Literal::U64(n) => ops.push(Operation::PushU64(n)),
                Literal::U8(n) => ops.push(Operation::PushU64(n as u64)),
                Literal::Bool(b) => ops.push(Operation::PushU64(b as u64)),
                _ => todo!("value: {:?}", value),
            },
            TypedExpr::Call { func } => ops.push(Operation::Call(func)),
            TypedExpr::As { args, block } => {
                for arg in &args {
                    ops.push(Operation::FromStackToFrame {
                        bytes: arg.size(types).unwrap(),
                    });
                }

                if let Some(block) = block {
                    for e in block {
                        ops.append(&mut Operation::from_expr(
                            e,
                            types,
                            data,
                            jump_count,
                            frame_reserved,
                        ));
                    }

                    for arg in &args {
                        ops.push(Operation::PopFramed {
                            bytes: arg.size(types).unwrap(),
                        })
                    }
                }
            }
            TypedExpr::Cast { .. } => (),
            TypedExpr::Var { typ, dimension } => {
                let mut size = typ.size(types).unwrap();
                if let Some(dimension) = dimension {
                    size *= dimension;
                }

                *frame_reserved += size;
            }
            TypedExpr::While { cond, body } => {
                let while_dest = *jump_count;
                ops.push(Operation::JumpDest { id: while_dest });
                *jump_count += 1;
                for e in cond {
                    ops.append(&mut Operation::from_expr(
                        e,
                        types,
                        data,
                        jump_count,
                        frame_reserved,
                    ));
                }

                let mut body_ops = vec![];

                for e in body {
                    body_ops.append(&mut Operation::from_expr(
                        e,
                        types,
                        data,
                        jump_count,
                        frame_reserved,
                    ));
                }

                ops.push(Operation::JumpFalse {
                    dest_id: *jump_count,
                });

                ops.append(&mut body_ops);

                ops.push(Operation::Jump {
                    dest_id: while_dest,
                });

                ops.push(Operation::JumpDest { id: *jump_count });
                *jump_count += 1;
            }
            TypedExpr::SizeOf { typ } => {
                ops.push(Operation::PushU64(typ.size(types).unwrap() as u64))
            }
            TypedExpr::Syscall { n } => ops.push(Operation::Syscall(n)),
            e => unimplemented!("{:?}", e),
        }

        ops
    }

    pub fn from_function(func: Type, types: &BTreeMap<TypeId, Type>) -> Vec<Self> {
        if let Type::Function {
            body, inputs, name, ..
        } = func
        {
            println!("{name}");
            let mut ops = vec![Operation::StartBlock];

            for Arg { ident, typ, .. } in inputs.iter().rev() {
                if ident.is_some() {
                    ops.push(Operation::FromStackToFrame {
                        bytes: typ.0.size(types).unwrap(),
                    });
                }
            }

            let mut reserve = 0;
            let mut data = HashMap::new();
            for expr in body {
                ops.append(&mut Operation::from_expr(
                    expr,
                    types,
                    &mut data,
                    &mut 0,
                    &mut reserve,
                ));
            }

            ops.push(Operation::EndBlock);

            let mut out = vec![Operation::FrameReserve { bytes: reserve }];
            out.append(&mut ops);

            out
        } else {
            panic!("Can only create backend operations from functions!");
        }
    }
}
