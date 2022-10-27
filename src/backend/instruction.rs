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
pub enum Instruction {
    Call(String),
    PushU64(u64),
    PushGlobal {
        id: String,
    },
    PushFromFrame {
        offset_from_end: usize,
        bytes: usize,
    },
    FramePtrToFrameReserve {
        offset: usize,
    },
    PushToFrame {
        bytes: usize,
    },
    Operator {
        op: Operator,
    },
    JumpDest {
        id: usize,
    },
    JumpFalse {
        dest_id: usize,
    },
    Jump {
        dest_id: usize,
    },
    Syscall(usize),
    StartBlock,
    EndBlock {
        bytes_to_free: usize,
    },
    FrameReserve {
        bytes: usize,
    },
    InitLocalVarArr {
        offset_to_var: usize,
        offset_to_data: usize,
        data_len: usize,
    },
}

impl Instruction {
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
                for (_, tid) in &frame[idx..] {
                    offset += tid.size(types).unwrap();
                }

                let mut typ = &frame[idx].1;
                let bytes = if let Some(inner) = &inner {
                    for inner in inner {
                        typ = if let Type::Record { members, kind, .. } = types.get(&typ).unwrap() {
                            match kind {
                                RecordKind::Struct => {
                                    let idx = members
                                        .iter()
                                        .enumerate()
                                        .find(|(_, m)| &m.ident.lexeme == inner)
                                        .unwrap()
                                        .0;

                                    members
                                        .iter()
                                        .rev()
                                        .take(members.len() - idx - 1)
                                        .for_each(|m| offset -= m.typ.0.size(types).unwrap());

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

                let i = Instruction::PushFromFrame {
                    offset_from_end: offset,
                    bytes,
                };

                ops.push(i);
            }
            TypedExpr::Operator { op } => ops.push(Instruction::Operator { op }),
            TypedExpr::If {
                then,
                otherwise,
                finally,
            } => {
                let mut n_jumps = 0;
                let mut then_ops = vec![];
                for e in then {
                    then_ops.push(Instruction::StartBlock);
                    then_ops.append(&mut Instruction::from_expr(
                        e,
                        types,
                        data,
                        jump_count,
                        frame_reserved,
                    ));
                    then_ops.push(Instruction::EndBlock {
                        bytes_to_free: Instruction::count_framed_bytes(&then_ops),
                    });
                }

                let mut otherwise_ops = vec![];
                for other in otherwise {
                    if let TypedExpr::ElseIf { condition, block } = other {
                        let mut cnd_ops = vec![];
                        let mut blk_ops = vec![];

                        for e in condition {
                            cnd_ops.push(Instruction::StartBlock);
                            cnd_ops.append(&mut Instruction::from_expr(
                                e,
                                types,
                                data,
                                jump_count,
                                frame_reserved,
                            ));
                            cnd_ops.push(Instruction::EndBlock {
                                bytes_to_free: Instruction::count_framed_bytes(&cnd_ops),
                            });
                        }

                        for e in block {
                            blk_ops.push(Instruction::StartBlock);
                            blk_ops.append(&mut Instruction::from_expr(
                                e,
                                types,
                                data,
                                jump_count,
                                frame_reserved,
                            ));
                            blk_ops.push(Instruction::EndBlock {
                                bytes_to_free: Instruction::count_framed_bytes(&blk_ops),
                            });
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
                        finally_ops.push(Instruction::StartBlock);
                        finally_ops.append(&mut Instruction::from_expr(
                            e,
                            types,
                            data,
                            jump_count,
                            frame_reserved,
                        ));
                        finally_ops.push(Instruction::EndBlock {
                            bytes_to_free: Instruction::count_framed_bytes(&finally_ops),
                        });
                    }
                    n_jumps += 1;
                }

                ops.push(Instruction::JumpFalse {
                    dest_id: *jump_count,
                });
                ops.append(&mut then_ops);
                ops.push(Instruction::Jump {
                    dest_id: *jump_count + n_jumps,
                });
                ops.push(Instruction::JumpDest { id: *jump_count });
                *jump_count += 1;

                for (mut cond, mut block) in otherwise_ops {
                    n_jumps -= 1;

                    ops.append(&mut cond);
                    ops.push(Instruction::JumpFalse {
                        dest_id: *jump_count,
                    });
                    ops.append(&mut block);
                    ops.push(Instruction::Jump {
                        dest_id: *jump_count + n_jumps,
                    });
                    ops.push(Instruction::JumpDest { id: *jump_count });
                    *jump_count += 1;
                }

                if !finally_ops.is_empty() {
                    n_jumps -= 1;
                    ops.append(&mut finally_ops);
                    ops.push(Instruction::Jump {
                        dest_id: *jump_count + n_jumps,
                    });
                    ops.push(Instruction::JumpDest { id: *jump_count });
                    *jump_count += 1;
                }
            }
            TypedExpr::Literal { value } => match value {
                Literal::String(s) => {
                    let n = data.len();
                    let str_len = s.len() as u64;
                    data.insert(n, Data::String(s));
                    ops.push(Instruction::PushU64(str_len));
                    ops.push(Instruction::PushGlobal {
                        id: format!("str_{n}"),
                    });
                }
                Literal::U64(n) => ops.push(Instruction::PushU64(n)),
                Literal::U8(n) => ops.push(Instruction::PushU64(n as u64)),
                Literal::Bool(b) => ops.push(Instruction::PushU64(b as u64)),
                _ => todo!("value: {:?}", value),
            },
            TypedExpr::Call { func } => ops.push(Instruction::Call(func)),
            TypedExpr::As { args, block } => {
                for arg in &args {
                    ops.push(Instruction::PushToFrame {
                        bytes: arg.size(types).unwrap(),
                    });
                }

                if let Some(block) = block {
                    let start = ops.len();
                    ops.push(Instruction::StartBlock);
                    for e in block {
                        ops.append(&mut Instruction::from_expr(
                            e,
                            types,
                            data,
                            jump_count,
                            frame_reserved,
                        ));
                    }
                    let to_release = Instruction::count_framed_bytes(&ops[start..]);
                    ops.push(Instruction::EndBlock {
                        bytes_to_free: to_release,
                    });
                }
            }
            TypedExpr::Cast { .. } => (),
            TypedExpr::Var { typ, data } => {
                // Space is reserved for all local var's within a function's scope.
                // Space is reserved in parsing order.
                // This is what the frame should look like before any execution
                // starts (including pushing args onto frame if named).
                //
                // eg.
                // ```
                // fn foo(char: c) {
                //     123 as [n]
                //     var u64[10]: bar
                //     var bool:    baz
                // }
                // ```
                //
                // | [Arr<u64>][ u64; 10 ][bool] |[char][u64][*Arr<u64>][*bool]
                // |    bar_    bar_data   baz_  |   c    n       bar     baz
                // |                             ^
                // |     Reserved space     frame start
                //
                // Note: the pointer to the var is pushed when the `var` expr
                //       is evaluated.

                // reserve space for typ
                let offset_to_var = *frame_reserved;
                *frame_reserved += typ;

                // reserve space for data (if any)
                if let Some(data) = data {
                    ops.push(Instruction::InitLocalVarArr {
                        offset_to_var,
                        offset_to_data: *frame_reserved,
                        data_len: data,
                    });
                    *frame_reserved += data;
                }

                // push ptr to typ onto frame.
                ops.push(Instruction::FramePtrToFrameReserve {
                    offset: offset_to_var,
                })
            }
            TypedExpr::While { cond, body } => {
                let while_dest = *jump_count;
                ops.push(Instruction::JumpDest { id: while_dest });
                *jump_count += 1;
                for e in cond {
                    ops.append(&mut Instruction::from_expr(
                        e,
                        types,
                        data,
                        jump_count,
                        frame_reserved,
                    ));
                }

                let mut body_ops = vec![];

                for e in body {
                    let start = ops.len();
                    ops.push(Instruction::StartBlock);
                    body_ops.append(&mut Instruction::from_expr(
                        e,
                        types,
                        data,
                        jump_count,
                        frame_reserved,
                    ));

                    ops.push(Instruction::EndBlock {
                        bytes_to_free: Instruction::count_framed_bytes(&ops[start..]),
                    });
                }

                ops.push(Instruction::JumpFalse {
                    dest_id: *jump_count,
                });

                ops.append(&mut body_ops);

                ops.push(Instruction::Jump {
                    dest_id: while_dest,
                });

                ops.push(Instruction::JumpDest { id: *jump_count });
                *jump_count += 1;
            }
            TypedExpr::SizeOf { typ } => {
                ops.push(Instruction::PushU64(typ.size(types).unwrap() as u64))
            }
            TypedExpr::Syscall { n } => ops.push(Instruction::Syscall(n)),
            e => unimplemented!("{:?}", e),
        }

        ops
    }

    pub fn from_function(func: Type, types: &BTreeMap<TypeId, Type>) -> Vec<Self> {
        if let Type::Function { body, inputs, .. } = func {
            let mut ops = vec![
                Instruction::FrameReserve { bytes: 0 },
                Instruction::StartBlock,
            ];

            for Arg { ident, typ, .. } in inputs.iter().rev() {
                if ident.is_some() {
                    ops.push(Instruction::PushToFrame {
                        bytes: typ.0.size(types).unwrap(),
                    });
                }
            }

            let mut reserve = 0;
            let mut data = HashMap::new();
            for expr in body {
                ops.append(&mut Instruction::from_expr(
                    expr,
                    types,
                    &mut data,
                    &mut 0,
                    &mut reserve,
                ));
            }

            let bytes_framed = Instruction::count_framed_bytes(&ops);

            ops.push(Instruction::EndBlock {
                bytes_to_free: bytes_framed + reserve,
            });

            ops[0] = Instruction::FrameReserve { bytes: reserve };

            ops
        } else {
            panic!("Can only create backend Instructions from functions!");
        }
    }

    pub fn count_framed_bytes(instrs: &[Instruction]) -> usize {
        let mut framed_bytes = 0;
        for i in instrs {
            match i {
                Instruction::PushToFrame { bytes } => framed_bytes += bytes,
                Instruction::FramePtrToFrameReserve { .. } => framed_bytes += 1,
                Instruction::EndBlock { bytes_to_free } => framed_bytes -= bytes_to_free,
                _ => (),
            }
        }

        framed_bytes
    }
}
