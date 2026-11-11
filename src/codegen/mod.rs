use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
    format, matches, todo, unreachable, vec, writeln,
};

use crate::{
    lex::token::Token,
    typecheck::{
        expr::Var,
        typ::{
            typ::{BuiltinType, EnumDef, EnumType, Mutability, Type, VariantType},
            type_id::{EnumId, StructId, TypeId},
        },
        typestore::TypeStore,
        TypedFunction,
    },
};

pub struct CCodeGen<'a, 'src, W>
where
    W: Write,
{
    typestore: &'a mut TypeStore<Token<'src>>,
    types: &'a Vec<TypeId>,
    fns: &'a [(Var, TypedFunction<'src>)],
    w: &'a mut W,
}

impl<'a, 'src, W> CCodeGen<'a, 'src, W>
where
    W: Write,
{
    fn format_enum_id(eid: EnumId, tid: TypeId) -> String {
        format!("haystack_enum_{}_{}", eid.0, tid.0)
    }

    fn format_enum_tag(eid: EnumId, tid: TypeId, tag: usize) -> String {
        format!("{}_variant_{tag}", Self::format_enum_id(eid, tid))
    }

    fn format_struct_id(sid: StructId, tid: TypeId) -> String {
        format!("haystack_struct_{}_{}", sid.0, tid.0)
    }

    fn format_tid(tid: TypeId) -> String {
        format!("tid_{}", tid.0)
    }

    fn format_function_input(tid: TypeId) -> String {
        format!("{}_input", Self::format_tid(tid))
    }

    fn format_function_output(tid: TypeId) -> String {
        format!("{}_output", Self::format_tid(tid))
    }

    fn format_metatype(tid: TypeId) -> String {
        format!("TYPE_{}", tid.0)
    }

    fn format_metatype_tid() -> String {
        String::from("metatype")
    }

    fn forward_declare_tid(&mut self, tid: &TypeId) -> std::fmt::Result {
        match self.typestore.get(*tid).unwrap() {
            Type::TypeVar(_) => todo!(),
            Type::RigidTypeVar(_) => todo!(),
            Type::Builtin(_) => (),
            Type::Function(f) => {
                if (!f.input.is_empty()) {
                    writeln!(self.w, "struct {};", Self::format_function_input(*tid))?;
                }
                if !f.output.is_empty() {
                    writeln!(self.w, "struct {};", Self::format_function_output(*tid))?;
                }
            }
            Type::Pointer(_) => (),
            Type::Struct(s) => {
                writeln!(self.w, "struct {};", Self::format_struct_id(s.sid, *tid))?;
            }
            Type::Enum(e) => {
                writeln!(self.w, "struct {};", Self::format_enum_id(e.eid, *tid))?;
            }
            Type::Tuple(_) => todo!(),
            Type::Variant(_) => (),
            Type::Type(_) => (),
            Type::Projection(_) => todo!(),
            Type::Never => todo!(),
        }
        Ok(())
    }

    fn forward_declare_types(&mut self) -> std::fmt::Result {
        for tid in self.types {
            self.forward_declare_tid(tid)?;
        }

        Ok(())
    }

    fn declare_function_input(&mut self, tid: TypeId, input: &[TypeId]) -> std::fmt::Result {
        if input.is_empty() {
            return writeln!(self.w, "typedef void {};", Self::format_function_input(tid));
        }

        writeln!(
            self.w,
            "typedef struct {} {{",
            Self::format_function_input(tid)
        )?;
        for (idx, tid) in input.iter().enumerate() {
            writeln!(self.w, "    {} _{idx};", Self::format_tid(*tid))?;
        }
        writeln!(self.w, "}} {};", Self::format_function_input(tid))?;
        Ok(())
    }

    fn declare_function_output(&mut self, tid: TypeId, output: &[TypeId]) -> std::fmt::Result {
        if output.is_empty() {
            return writeln!(self.w, "typedef void {};", Self::format_function_output(tid));
        }

        writeln!(
            self.w,
            "typedef struct {} {{",
            Self::format_function_output(tid)
        )?;
        for (idx, tid) in output.iter().enumerate() {
            writeln!(self.w, "    {} _{idx};", Self::format_tid(*tid))?;
        }
        writeln!(self.w, "}} {};", Self::format_function_output(tid))?;
        Ok(())
    }

    fn declare_tid(&mut self, tid: TypeId) -> std::fmt::Result {
        match self.typestore.get(tid).unwrap().clone() {
            Type::TypeVar(_) | Type::RigidTypeVar(_) => unreachable!(),
            Type::Builtin(builtin_type) => {
                let c_type = match builtin_type {
                    BuiltinType::U8 => "uint8_t",
                    BuiltinType::U64 => "uint64_t",
                    BuiltinType::Bool => "bool",
                    BuiltinType::CVarArgs => return Ok(()),
                };

                writeln!(self.w, "typedef {c_type} {};", Self::format_tid(tid));
            }
            Type::Function(f) => {
                self.declare_function_input(tid, &f.input)?;
                self.declare_function_output(tid, &f.output)?;

                writeln!(
                    self.w,
                    "typedef {} (*{})({});",
                    Self::format_function_output(tid),
                    Self::format_tid(tid),
                    Self::format_function_input(tid)
                )?;
            }
            Type::Pointer(p) => {
                let modifier = if p.mutability == Mutability::Mutable {
                    ""
                } else {
                    "const "
                };

                writeln!(
                    self.w,
                    "typedef {modifier}{}* {};",
                    Self::format_tid(p.tid),
                    Self::format_tid(tid)
                )?;
            }
            Type::Struct(s) => {
                let sid = s.sid;
                let members = self.typestore.get_struct_members(s);

                writeln!(
                    self.w,
                    "typedef struct {} {{",
                    Self::format_struct_id(sid, tid)
                )?;
                for (tag, tid) in members.iter().enumerate() {
                    writeln!(self.w, "    {} _{tag};", Self::format_tid(*tid))?;
                }
                writeln!(self.w, "}} {};", Self::format_tid(tid));
            }
            Type::Enum(e) => {
                let eid = e.eid;
                let variants = self.typestore.get_enum_variants(e.eid, e.args.clone());

                writeln!(
                    self.w,
                    "typedef struct {} {{",
                    Self::format_enum_id(eid, tid)
                )?;
                writeln!(self.w, "    enum {{")?;
                for (tag, _) in variants.iter().enumerate() {
                    writeln!(self.w, "        {},", Self::format_enum_tag(eid, tid, tag))?;
                }
                writeln!(self.w, "    }} tag;")?;
                writeln!(self.w, "    union {{")?;
                for (tag, tid) in variants.iter().enumerate() {
                    if let Some(tid) = tid {
                        writeln!(self.w, "        {} _{tag};", Self::format_tid(*tid))?;
                    }
                }
                writeln!(self.w, "    }} value;")?;
                writeln!(self.w, "}} {};", Self::format_tid(tid));
            }
            Type::Tuple(type_ids) => todo!(),
            Type::Variant(v) => {
                let parent_tid = self.typestore.intern(Type::Enum(EnumType {
                    eid: v.eid,
                    args: v.args,
                }));

                writeln!(
                    self.w,
                    "typedef {} {};",
                    Self::format_tid(parent_tid),
                    Self::format_tid(tid),
                )?;
            }
            Type::Type(_) => {
                writeln!(
                    self.w,
                    "typedef {} {};",
                    Self::format_metatype_tid(),
                    Self::format_tid(tid)
                );
            }
            Type::Projection(projection) => todo!(),
            Type::Never => todo!(),
        }

        Ok(())
    }

    fn declare_metatypes(&mut self, metatypes: &[&TypeId]) -> std::fmt::Result {
        writeln!(self.w, "typedef enum {{")?;
        for tid in metatypes {
            writeln!(self.w, "    {},", Self::format_metatype(**tid))?;
        }
        writeln!(self.w, "}} {};", Self::format_metatype_tid())?;
        Ok(())
    }

    fn declare_types(&mut self) -> std::fmt::Result {
        let metatypes: Vec<&TypeId> = self
            .types
            .iter()
            .filter(|tid| matches!(self.typestore.get(**tid), Some(Type::Type(_))))
            .collect();
        self.declare_metatypes(&metatypes)?;
        for tid in self.types {
            self.declare_tid(*tid)?;
        }

        Ok(())
    }

    fn type_dependencies(typestore: &mut TypeStore<Token<'_>>, tid: TypeId) -> Vec<TypeId> {
        match typestore.get(tid).unwrap() {
            Type::TypeVar(_) | Type::RigidTypeVar(_) => unreachable!(),
            Type::Builtin(_) => vec![],
            Type::Function(f) => {
                let f = f.clone();
                f.input.into_iter().chain(f.output.into_iter()).collect()
            }
            Type::Pointer(p) => {
                vec![p.tid]
            }
            Type::Struct(s) => typestore.get_struct_members(s.clone()),
            Type::Enum(e) => typestore
                .get_enum_variants(e.eid, e.args.clone())
                .into_iter()
                .filter_map(|maybe_tid| maybe_tid)
                .collect(),
            Type::Tuple(tids) => tids.clone(),
            Type::Variant(v) => {
                let v = v.clone();
                let parent_tid = typestore.intern(Type::Enum(EnumType {
                    eid: v.eid,
                    args: v.args.clone(),
                }));
                typestore
                    .get_enum_variants(v.eid, v.args.clone())
                    .into_iter()
                    .filter_map(|maybe_tid| maybe_tid)
                    .chain([parent_tid].into_iter())
                    .collect()
            }
            Type::Type(tid) => vec![*tid],
            Type::Projection(projection) => todo!(),
            Type::Never => todo!(),
        }
    }

    fn visit_tid(
        typestore: &mut TypeStore<Token<'_>>,
        result: &mut Vec<TypeId>,
        stack: &mut Vec<TypeId>,
        state: &mut HashMap<TypeId, VisitState>,
        tid: TypeId,
    ) -> Result<(), ()> {
        match state.get(&tid) {
            Some(s) => match s {
                VisitState::Visited => return Ok(()),
                VisitState::Visiting => {
                    // cycle detected
                    let e = ();
                    return Err(e);
                }
            },
            None => (),
        };

        state.insert(tid, VisitState::Visiting);
        stack.push(tid);

        for dep in Self::type_dependencies(typestore, tid) {
            eprintln!("{tid:?}({}) depends on {dep:?}", typestore.fmt(tid));
            Self::visit_tid(typestore, result, stack, state, dep)?;
        }

        stack.pop();
        state.insert(tid, VisitState::Visited);

        result.push(tid);

        Ok(())
    }

    fn order_types(typestore: &mut TypeStore<Token<'_>>, types: &[TypeId]) -> Vec<TypeId> {
        let mut result = vec![];

        let mut stack = vec![];
        let mut visited = HashMap::new();
        for tid in types {
            Self::visit_tid(typestore, &mut result, &mut stack, &mut visited, *tid).unwrap();
        }

        result
    }

    pub fn generate(
        w: &'a mut W,
        typestore: &'a mut TypeStore<Token<'src>>,
        types: &'a Vec<TypeId>,
        fns: &'a [(Var, TypedFunction<'src>)],
    ) -> std::fmt::Result {
        let types = Self::order_types(typestore, types);
        for tid in &types {
            eprintln!("{tid:?}");
        }
        let mut gen = CCodeGen {
            typestore,
            types: &types,
            fns,
            w,
        };

        writeln!(&mut gen.w, "#include <stdint.h>")?;
        writeln!(&mut gen.w, "#include <stdbool.h>")?;
        gen.forward_declare_types()?;
        writeln!(&mut gen.w);
        gen.declare_types()?;

        Ok(())
    }
}

enum VisitState {
    Visiting,
    Visited,
}
