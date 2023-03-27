use std::collections::{BTreeMap, HashMap};

use crate::{
    ast::{arg::TypedArg, expr::TypedExpr},
    lex::token::{Literal, Operator},
    types::{FnTag, FramedType, Function, RecordKind, Type, TypeId, TypeMap},
};

#[derive(Debug, Clone)]
pub enum InitData {
    String(String),
    Arr { size: usize, pointer: String },
}

pub enum UninitData {
    Region(usize),
}

pub type InitDataMap = HashMap<String, InitData>;
pub type UninitDataMap = HashMap<String, UninitData>;

#[derive(Debug, Clone)]
pub enum Instruction {
    Call(String),
    Return,
    PushU64(u64),
    PushGlobal {
        id: String,
    },
    PushFromFrame {
        offset_from_end: usize,
        bytes: usize,
    },
    PushPtrToFrame {
        offset_from_end: usize,
    },
    FramePtrToFrameReserve {
        offset: usize,
        size: usize,
        width: usize,
    },
    PushToFrame {
        quad_words: usize,
    },
    Operator {
        op: Operator,
        size: Option<(usize, usize)>,
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
        data_size: usize,
        data_width: usize,
    },
}

impl Instruction {
    fn escape_string(s: String) -> String {
        let mut new_s = String::new();
        let mut i = 0;

        while i < s.len() {
            match s.chars().nth(i).unwrap() {
                '\\' => {
                    i += 1;
                    match s.chars().nth(i).unwrap() {
                        'n' => new_s.push('\n'),
                        't' => new_s.push('\t'),
                        'r' => new_s.push('\r'),
                        '0' => new_s.push('\0'),
                        '\\' => new_s.push('\\'),
                        _ => unreachable!(),
                    }
                }
                c => new_s.push(c),
            }

            i += 1;
        }
        new_s
    }

    fn from_expr(
        expr: TypedExpr,
        types: &BTreeMap<TypeId, Type>,
        init_data: &mut HashMap<String, InitData>,
        jump_count: &mut usize,
        frame_reserved: &mut usize,
    ) -> Vec<Self> {
        let mut ops = vec![];

        match expr {
            TypedExpr::Block { exprs } => {
                let start = ops.len();
                ops.push(Instruction::StartBlock);
                for e in exprs {
                    ops.append(&mut Instruction::from_expr(
                        e,
                        types,
                        init_data,
                        jump_count,
                        frame_reserved,
                    ))
                }
                let to_release = Instruction::count_framed_bytes(&ops[start..]);
                ops.push(Instruction::EndBlock {
                    bytes_to_free: to_release,
                });
            }
            TypedExpr::Return => ops.push(Instruction::Return),
            TypedExpr::Framed { frame, idx, inner } => {
                assert!(idx < frame.len());
                let mut offset = 0;
                for (_, FramedType { typ, .. }) in &frame[idx + 1..] {
                    offset += typ.size(types).unwrap();
                }

                let mut typ = &frame[idx].1.typ;
                let bytes = if let Some(inner) = &inner {
                    if inner.is_empty() {
                        match types.get(typ).unwrap() {
                            Type::Record {kind: RecordKind::EnumStruct, ..} => {
                                offset += typ.size(types).unwrap() - 1;
                                1
                            },
                            _ => unreachable!("Internal Error: framed inner should only be empty on Enum Struct types"),
                        }
                    } else {
                        for inner in inner {
                            typ = match types.get(typ).unwrap() {
                                Type::Record { members, kind, .. } => match kind {
                                    RecordKind::Struct => {
                                        let idx = members
                                            .iter()
                                            .enumerate()
                                            .find(|(_, m)| &m.ident.lexeme == inner)
                                            .unwrap()
                                            .0;

                                        for m in &members[0..idx] {
                                            offset += m.typ.size(types).unwrap()
                                        }

                                        &members[idx].typ
                                    }
                                    RecordKind::Union => {
                                        let idx = members
                                            .iter()
                                            .enumerate()
                                            .find(|(_, m)| &m.ident.lexeme == inner)
                                            .unwrap()
                                            .0;

                                        &members[idx].typ
                                    }
                                    RecordKind::EnumStruct => {
                                        let idx = inner.parse::<usize>().expect(
                                            "Internal error - this should be a formatted usize",
                                        );
                                        &members[idx].typ
                                    }
                                    RecordKind::Interface => unreachable!(),
                                    RecordKind::Tuple => unreachable!(),
                                },
                                Type::Tuple {
                                    inner: tuple_members,
                                    idents: None,
                                } => {
                                    let idx = inner
                                        .parse::<usize>()
                                        .unwrap_or_else(|_| panic!("{inner} should be a usize"));
                                    for m in &tuple_members[0..idx] {
                                        offset += m.size(types).unwrap()
                                    }
                                    &tuple_members[idx]
                                }
                                Type::Tuple {
                                    inner: tuple_members,
                                    idents: Some(idents),
                                } => {
                                    let idx = idents
                                        .iter()
                                        .enumerate()
                                        .find(|(_, id)| &id.lexeme == inner)
                                        .unwrap()
                                        .0;

                                    for m in &tuple_members[0..idx] {
                                        offset += m.size(types).unwrap()
                                    }

                                    &tuple_members[idx]
                                }
                                _ => panic!("Didn't expect to find {typ} here!"),
                            }
                        }
                        typ.size(types).unwrap()
                    }
                } else {
                    typ.size(types).unwrap()
                };

                ops.push(Instruction::PushFromFrame {
                    offset_from_end: offset,
                    bytes,
                });
            }
            TypedExpr::AddrFramed { frame, idx, inner } => {
                assert!(idx < frame.len());
                let mut offset = 0;
                for (_, FramedType { typ, .. }) in &frame[idx + 1..] {
                    offset += typ.size(types).unwrap();
                }

                let mut typ = &frame[idx].1.typ;
                if let Some(inner) = &inner {
                    for inner in inner {
                        typ = match types.get(typ).unwrap() {
                            Type::Record { members, kind, .. } => match kind {
                                RecordKind::Struct => {
                                    let idx = members
                                        .iter()
                                        .enumerate()
                                        .find(|(_, m)| &m.ident.lexeme == inner)
                                        .unwrap()
                                        .0;

                                    for m in &members[0..idx] {
                                        offset += m.typ.size(types).unwrap()
                                    }

                                    &members[idx].typ
                                }
                                RecordKind::Union => {
                                    let idx = members
                                        .iter()
                                        .enumerate()
                                        .find(|(_, m)| &m.ident.lexeme == inner)
                                        .unwrap()
                                        .0;

                                    &members[idx].typ
                                }
                                RecordKind::EnumStruct => todo!(),
                                RecordKind::Interface => unreachable!(),
                                RecordKind::Tuple => unreachable!(),
                            },
                            Type::Tuple {
                                inner: tuple_inner,
                                idents: Some(idents),
                            } => {
                                let idx = idents
                                    .iter()
                                    .enumerate()
                                    .find(|(_, m)| &m.lexeme == inner)
                                    .unwrap()
                                    .0;

                                for m in &tuple_inner[0..idx] {
                                    offset += m.size(types).unwrap()
                                }

                                &tuple_inner[idx]
                            }
                            Type::Tuple {
                                inner: tuple_inner,
                                idents: None,
                            } => {
                                let idx = inner.parse::<usize>().unwrap();
                                for m in &tuple_inner[0..idx] {
                                    offset += m.size(types).unwrap()
                                }

                                &tuple_inner[idx]
                            }
                            _ => panic!("{typ}"),
                        }
                    }
                };

                ops.push(Instruction::PushPtrToFrame {
                    offset_from_end: offset,
                });
            }
            TypedExpr::FramedPointerOffset { frame, idx, inner } => {
                assert!(idx < frame.len());
                let mut frame_offset = 0;
                let mut ptr_offset = 0;
                for (_, FramedType { typ: tid, .. }) in &frame[idx + 1..] {
                    frame_offset += tid.size(types).unwrap();
                }

                let mut typ = &frame[idx].1.typ;
                if let Type::Pointer {
                    inner: inner_ptr_tid,
                    ..
                } = types.get(typ).unwrap()
                {
                    typ = inner_ptr_tid;

                    for inner in inner {
                        typ = if let Type::Record { members, kind, .. } = types.get(typ).unwrap() {
                            match kind {
                                RecordKind::Struct => {
                                    let idx = members
                                        .iter()
                                        .enumerate()
                                        .find(|(_, m)| m.ident.lexeme == inner)
                                        .unwrap()
                                        .0;

                                    for m in &members[0..idx] {
                                        ptr_offset += m.typ.size(types).unwrap()
                                    }

                                    &members[idx].typ
                                }
                                RecordKind::Union => {
                                    let idx = members
                                        .iter()
                                        .enumerate()
                                        .find(|(_, m)| m.ident.lexeme == inner)
                                        .unwrap()
                                        .0;

                                    &members[idx].typ
                                }
                                RecordKind::EnumStruct => todo!(),
                                RecordKind::Interface => unreachable!(),
                                RecordKind::Tuple => unreachable!(),
                            }
                        } else {
                            panic!("{typ}");
                        }
                    }
                } else {
                    panic!("Expected a pointer type. Found {typ}");
                }

                ops.push(Instruction::PushFromFrame {
                    offset_from_end: frame_offset,
                    bytes: frame[idx].1.typ.size(types).unwrap(),
                });
                ops.push(Instruction::PushU64(ptr_offset as u64 * 8));
                ops.push(Instruction::Operator {
                    op: Operator::Plus,
                    size: None,
                })
            }
            TypedExpr::Operator { op, typ: Some(typ) } => ops.push(Instruction::Operator {
                op,
                size: Some((typ.size(types).unwrap(), typ.width())),
            }),
            TypedExpr::Operator { op, typ: None } => {
                ops.push(Instruction::Operator { op, size: None })
            }
            TypedExpr::If {
                then,
                otherwise,
                finally,
            } => {
                let mut n_jumps = 0;
                let mut then_ops = vec![];
                then_ops.push(Instruction::StartBlock);
                then_ops.append(&mut Instruction::from_expr(
                    *then,
                    types,
                    init_data,
                    jump_count,
                    frame_reserved,
                ));

                then_ops.push(Instruction::EndBlock {
                    bytes_to_free: Instruction::count_framed_bytes(&then_ops),
                });

                let mut otherwise_ops = vec![];
                for other in otherwise {
                    if let TypedExpr::ElseIf { condition, block } = other {
                        let mut cnd_ops = vec![];
                        let mut blk_ops = vec![];
                        cnd_ops.push(Instruction::StartBlock);
                        for e in condition {
                            cnd_ops.append(&mut Instruction::from_expr(
                                e,
                                types,
                                init_data,
                                jump_count,
                                frame_reserved,
                            ));
                        }
                        cnd_ops.push(Instruction::EndBlock {
                            bytes_to_free: Instruction::count_framed_bytes(&cnd_ops),
                        });

                        blk_ops.append(&mut Instruction::from_expr(
                            *block,
                            types,
                            init_data,
                            jump_count,
                            frame_reserved,
                        ));

                        blk_ops.push(Instruction::EndBlock {
                            bytes_to_free: Instruction::count_framed_bytes(&blk_ops),
                        });

                        otherwise_ops.push((cnd_ops, blk_ops))
                    } else {
                        panic!("{other:?}");
                    }
                    n_jumps += 1;
                }

                let mut finally_ops = vec![];
                if let Some(finally) = finally {
                    finally_ops.push(Instruction::StartBlock);
                    finally_ops.append(&mut Instruction::from_expr(
                        *finally,
                        types,
                        init_data,
                        jump_count,
                        frame_reserved,
                    ));

                    finally_ops.push(Instruction::EndBlock {
                        bytes_to_free: Instruction::count_framed_bytes(&finally_ops),
                    });
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
                    let n = init_data.len();
                    let s = Instruction::escape_string(s);

                    let str_len = s.len() as u64;
                    init_data.insert(format!("str_{n}"), InitData::String(s));
                    ops.push(Instruction::PushU64(str_len));
                    ops.push(Instruction::PushGlobal {
                        id: format!("str_{n}"),
                    });
                }
                Literal::U64(n) => ops.push(Instruction::PushU64(n)),
                Literal::U8(n) => ops.push(Instruction::PushU64(n as u64)),
                Literal::Bool(b) => ops.push(Instruction::PushU64(b as u64)),
                Literal::Char(c) => ops.push(Instruction::PushU64(c as u64)),
            },
            TypedExpr::Call { func } => {
                let tid = TypeId::new(&func);
                let t = types.get(&tid).unwrap();
                let f = t.function();
                if !f.has_tag(FnTag::Inline) {
                    ops.push(Instruction::Call(func))
                } else {
                    ops.append(&mut Instruction::from_inlined_function(
                        f.clone(),
                        types,
                        init_data,
                        jump_count,
                        frame_reserved,
                    ))
                }
            }
            TypedExpr::As { args } => {
                for arg in &args {
                    ops.push(Instruction::PushToFrame {
                        quad_words: arg.size(types).unwrap(),
                    });
                }
            }
            TypedExpr::Cast { .. } | TypedExpr::Unpack { .. } => (),
            TypedExpr::CastEnumStruct { idx, padding, .. } => {
                for _ in 0..padding {
                    ops.push(Instruction::PushU64(0));
                }
                ops.push(Instruction::PushU64(idx as u64))
            }
            TypedExpr::Var { size, width, data } => {
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
                *frame_reserved += size * width;

                // reserve space for data (if any)
                if let Some((data_size, data_width)) = data {
                    ops.push(Instruction::InitLocalVarArr {
                        offset_to_var,
                        offset_to_data: *frame_reserved,
                        data_size,
                        data_width,
                    });
                    *frame_reserved += data_size * data_width;
                }

                // push ptr to typ onto frame.
                ops.push(Instruction::FramePtrToFrameReserve {
                    offset: offset_to_var,
                    size,
                    width,
                })
            }
            TypedExpr::While { cond, body } => {
                let while_start_dest = *jump_count;
                ops.push(Instruction::JumpDest {
                    id: while_start_dest,
                });
                *jump_count += 1;
                for e in cond {
                    ops.append(&mut Instruction::from_expr(
                        e,
                        types,
                        init_data,
                        jump_count,
                        frame_reserved,
                    ));
                }
                let while_end_dest = *jump_count;
                ops.push(Instruction::JumpFalse {
                    dest_id: while_end_dest,
                });
                *jump_count += 1;

                ops.push(Instruction::StartBlock);

                let mut body_ops =
                    Instruction::from_expr(*body, types, init_data, jump_count, frame_reserved);
                body_ops.push(Instruction::EndBlock {
                    bytes_to_free: Instruction::count_framed_bytes(&body_ops),
                });

                ops.append(&mut body_ops);

                ops.push(Instruction::Jump {
                    dest_id: while_start_dest,
                });

                ops.push(Instruction::JumpDest { id: while_end_dest });
                *jump_count += 1;
            }
            TypedExpr::Syscall { n } => ops.push(Instruction::Syscall(n)),
            TypedExpr::Global { ident } => ops.push(Instruction::PushGlobal { id: ident }),
            TypedExpr::ElseIf { .. } => todo!(),
            TypedExpr::Enum { typ, variant } => {
                if let Type::Enum { variants, .. } = types.get(&typ).unwrap() {
                    ops.push(Instruction::PushU64(
                        variants
                            .iter()
                            .enumerate()
                            .find(|(_, v)| v.lexeme == variant)
                            .unwrap()
                            .0 as u64,
                    ));
                } else {
                    panic!();
                }
            }
            TypedExpr::Tuple { exprs } => {
                for e in exprs {
                    ops.append(&mut Instruction::from_expr(
                        e,
                        types,
                        init_data,
                        jump_count,
                        frame_reserved,
                    ))
                }
            }
            TypedExpr::Pad { padding } => {
                for _ in 0..padding {
                    ops.push(Instruction::PushU64(0));
                }
            }
        }

        ops
    }

    fn from_inlined_function(
        func: Function,
        types: &TypeMap,
        init_data: &mut HashMap<String, InitData>,
        jump_count: &mut usize,
        frame_reserved: &mut usize,
    ) -> Vec<Self> {
        let mut ops = vec![Instruction::StartBlock];

        for TypedArg { ident, typ, .. } in func.inputs.iter().rev() {
            if ident.is_some() {
                ops.push(Instruction::PushToFrame {
                    quad_words: typ.size(types).unwrap(),
                });
            }
        }

        ops.append(&mut Instruction::from_expr(
            func.body,
            types,
            init_data,
            jump_count,
            frame_reserved,
        ));

        ops.push(Instruction::EndBlock {
            bytes_to_free: Instruction::count_framed_bytes(&ops),
        });

        ops
    }

    pub fn from_function(
        func: Function,
        types: &BTreeMap<TypeId, Type>,
        init_data: &mut HashMap<String, InitData>,
    ) -> Vec<Self> {
        let mut ops = vec![
            Instruction::FrameReserve { bytes: 0 },
            Instruction::StartBlock,
        ];

        for TypedArg { ident, typ, .. } in func.inputs.iter().rev() {
            if ident.is_some() {
                ops.push(Instruction::PushToFrame {
                    quad_words: typ.size(types).unwrap(),
                });
            }
        }

        let mut reserve = 0;
        let mut jump_count = 0;
        ops.append(&mut Instruction::from_expr(
            func.body,
            types,
            init_data,
            &mut jump_count,
            &mut reserve,
        ));

        let bytes_framed = Instruction::count_framed_bytes(&ops);

        ops.push(Instruction::EndBlock {
            bytes_to_free: bytes_framed + reserve,
        });
        ops.push(Instruction::Return);

        ops[0] = Instruction::FrameReserve { bytes: reserve };

        ops
    }

    pub fn from_type_map<'a>(
        types: &'a TypeMap,
        init_data: &mut InitDataMap,
    ) -> Vec<(&'a str, Vec<Self>)> {
        let mut instructions = vec![];
        types
            .iter()
            .filter(|(_, t)| matches!(t, Type::Function { .. }))
            .for_each(|(tid, func)| {
                if let Type::Function { func } = func {
                    instructions.push((
                        tid.0.as_str(),
                        Instruction::from_function(func.clone(), types, init_data),
                    ));
                } else {
                    panic!()
                }
            });
        instructions
    }

    pub fn count_framed_bytes(instrs: &[Instruction]) -> usize {
        let mut framed_bytes = 0;
        for i in instrs {
            match i {
                Instruction::PushToFrame { quad_words } => framed_bytes += quad_words * 8,
                Instruction::FramePtrToFrameReserve { .. } => framed_bytes += 8,
                Instruction::EndBlock { bytes_to_free } => framed_bytes -= bytes_to_free,
                _ => (),
            }
        }

        framed_bytes
    }
}
