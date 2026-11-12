use std::{
    collections::{HashMap, HashSet},
    eprintln,
    fmt::Write,
    format, matches, todo, unreachable, vec, writeln,
};

use crate::{
    lex::token::Token,
    typecheck::{
        expr::{Expr, MatchCase, MatchExpr, MatchPattern, Var},
        func,
        typ::{
            typ::{BuiltinType, EnumDef, EnumType, FunctionType, Mutability, Type, VariantType},
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
    ident_count: usize,
}

impl<'a, 'src, W> CCodeGen<'a, 'src, W>
where
    W: Write,
{
    fn format_stack_ident(n: usize) -> String {
        format!("anon_{n}")
    }

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

    fn format_var(var: Var) -> String {
        format!("haystack_var_{}", var.0)
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

    fn format_mutability(mutability: Mutability) -> &'static str {
        match mutability {
            Mutability::Mutable => "",
            Mutability::Const => "const ",
        }
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
            return writeln!(
                self.w,
                "typedef void {};",
                Self::format_function_output(tid)
            );
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
                let modifier = Self::format_mutability(p.mutability);

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
                eprintln!("Enum: {eid:?} -- Args: {:?}", &e.args);

                let variants = self.typestore.get_enum_variants(e.eid, e.args.clone());
                eprintln!("    {variants:?}");
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

    fn forward_declare_functions(&mut self) -> std::fmt::Result {
        for (var, func) in self.fns {
            let tid = func.expr.meta.1;
            writeln!(
                self.w,
                "{} {}({});",
                Self::format_function_output(tid),
                Self::format_var(*var),
                Self::format_function_input(tid)
            )?;
        }

        Ok(())
    }

    fn get_stack_effect(&self, tid: TypeId) -> FunctionType {
        let Type::Function(f) = self.typestore.get(tid).cloned().unwrap() else {
            unreachable!();
        };

        f
    }

    fn generate_input_binding(
        &mut self,
        tid: TypeId,
        input: &[TypeId],
        stack_idents: &mut Vec<usize>,
    ) -> Result<Option<usize>, std::fmt::Error> {
        if input.len() == 0 {
            return Ok(None);
        }

        let ident = self.next_stack_ident();
        writeln!(
            self.w,
            "    {} {} = {{",
            Self::format_function_input(tid),
            Self::format_stack_ident(ident)
        )?;
        for (idx, tid) in input.iter().enumerate().rev() {
            let ident = stack_idents.pop().unwrap();
            writeln!(
                self.w,
                "        ._{idx} = {},",
                Self::format_stack_ident(ident)
            )?;
        }
        writeln!(self.w, "    }};")?;
        Ok(Some(ident))
    }

    fn generate_output_unbinding(
        &mut self,
        output_ident: usize,
        output: &[TypeId],
        stack_idents: &mut Vec<usize>,
    ) -> std::fmt::Result {
        for (idx, tid) in output.iter().enumerate() {
            let ident = self.next_stack_ident();
            writeln!(
                self.w,
                "    const {} {} = {}._{idx};",
                Self::format_tid(*tid),
                Self::format_stack_ident(ident),
                Self::format_stack_ident(output_ident)
            )?;
            stack_idents.push(ident);
        }
        Ok(())
    }

    fn generate_function_call(
        &mut self,
        tid: TypeId,
        var: &Var,
        stack_idents: &mut Vec<usize>,
    ) -> std::fmt::Result {
        let func = self.get_stack_effect(tid);
        let input = self.generate_input_binding(tid, &func.input, stack_idents)?;

        let (output_prefix, output) = match func.output.len() {
            0 => (String::new(), None),
            _ => {
                let output = self.next_stack_ident();
                (
                    format!(
                        "const {} {} = ",
                        Self::format_function_output(tid),
                        Self::format_stack_ident(output)
                    ),
                    Some(output),
                )
            }
        };

        writeln!(
            self.w,
            "    {output_prefix}{}({});",
            Self::format_var(*var),
            match input {
                Some(input) => Self::format_stack_ident(input),
                None => "".into(),
            },
        )?;
        if let Some(output) = output {
            self.generate_output_unbinding(output, &func.output, stack_idents)?;
        }
        Ok(())
    }

    fn generate_match_case(
        &mut self,
        input: usize,
        tid: TypeId,
        case: &MatchCase<'src, (Token<'src>, TypeId)>,
        first: bool,
        stack_idents: &mut Vec<usize>,
    ) -> std::fmt::Result {
        let case_tid = case.expr.meta.1;
        let FunctionType {
            input: case_input,
            output: case_output,
        } = self.get_stack_effect(case_tid);
        match case.pattern {
            MatchPattern::Variant(variant_tid) => {
                let Type::Variant(variant) = self.typestore.get(variant_tid).unwrap().clone()
                else {
                    unreachable!()
                };

                writeln!(
                    self.w,
                    "    {}if ({}.tag == {}) {{",
                    if first { "" } else { "else " },
                    Self::format_stack_ident(input),
                    Self::format_enum_tag(variant.eid, tid, variant.variant)
                )?;

                if !case_input.is_empty() {
                    let ident = self.next_stack_ident();
                    stack_idents.push(ident);

                    writeln!(
                        self.w,
                        "    const {} {} = {}.value._{};",
                        Self::format_tid(case_input[0]),
                        Self::format_stack_ident(ident),
                        Self::format_stack_ident(input),
                        variant.variant
                    )?;
                }

                self.generate_expr(&case.expr, stack_idents)?;
                writeln!(self.w, "    }}")?;
            }
        }

        Ok(())
    }

    fn generate_match_expr(
        &mut self,
        tid: TypeId,
        match_expr: &MatchExpr<'src, (Token<'src>, TypeId)>,
        stack_idents: &mut Vec<usize>,
    ) -> std::fmt::Result {
        let enum_input = stack_idents.pop().unwrap();

        let FunctionType { input, output } = self.get_stack_effect(tid);

        assert!(input.len() == 1);

        let mut first = true;
        for case in &match_expr.cases {
            self.generate_match_case(enum_input, input[0], &case, first, stack_idents)?;
            first = false;
        }
        if let Some(finally) = &match_expr.finally {
            todo!();
        }

        Ok(())
    }

    fn generate_expr(
        &mut self,
        expr: &Expr<'src, (Token<'src>, TypeId)>,
        stack_idents: &mut Vec<usize>,
    ) -> std::fmt::Result {
        let tid = expr.meta.1;
        match &expr.expr {
            crate::typecheck::expr::ExprKind::U8(_) => todo!(),
            crate::typecheck::expr::ExprKind::U64(_) => todo!(),
            crate::typecheck::expr::ExprKind::Bool(b) => {
                let ident = self.next_stack_ident();
                stack_idents.push(ident);

                writeln!(
                    self.w,
                    "    const {} {} = {b};",
                    Self::format_tid(tid),
                    Self::format_stack_ident(ident)
                )?;
            }
            crate::typecheck::expr::ExprKind::String(s) => {
                let ident = self.next_stack_ident();

                let FunctionType { input, output } = self.get_stack_effect(tid);
                assert!(output.len() == 1);
                assert!(input.len() == 0);

                writeln!(
                    self.w,
                    "    const {} {} = {{",
                    Self::format_tid(output[0]),
                    Self::format_stack_ident(ident)
                )?;
                writeln!(self.w, "        ._0 = \"{s}\",")?;
                writeln!(self.w, "        ._1 = sizeof(\"{s}\"),")?;
                writeln!(self.w, "    }};")?;

                stack_idents.push(ident);
            }
            crate::typecheck::expr::ExprKind::Ident(var) => {
                match self.typestore.get(expr.meta.1).unwrap() {
                    Type::Function(_) => {
                        self.generate_function_call(tid, var, stack_idents)?;
                    }
                    _ => {
                        let ident = self.next_stack_ident();
                        stack_idents.push(ident);
                        writeln!(
                            self.w,
                            "    const {} {} = {};",
                            Self::format_tid(tid),
                            Self::format_stack_ident(ident),
                            Self::format_var(*var)
                        );
                    }
                }
            }
            crate::typecheck::expr::ExprKind::Ref { var, mutability } => {
                let FunctionType { input, output } = self.get_stack_effect(tid);
                assert!(input.len() == 0);
                assert!(output.len() == 1);
                let ident = self.next_stack_ident();
                writeln!(
                    self.w,
                    "    {}{}* {} = &{};",
                    Self::format_mutability(*mutability),
                    Self::format_tid(output[0]),
                    Self::format_stack_ident(ident),
                    Self::format_var(*var)
                )?;

                stack_idents.push(ident);
            }
            crate::typecheck::expr::ExprKind::Block(exprs) => {
                writeln!(self.w, "    {{")?;
                for expr in exprs {
                    self.generate_expr(expr, stack_idents)?;
                }
                writeln!(self.w, "    }}")?;
            }
            crate::typecheck::expr::ExprKind::Tuple(exprs) => todo!(),
            crate::typecheck::expr::ExprKind::Bind(vars) => {
                let FunctionType { input, output } = self.get_stack_effect(tid);
                assert!(input.len() == vars.len());
                assert!(output.len() == 0);
                assert!(vars.len() <= stack_idents.len());

                for (tid, var) in input.into_iter().zip(vars.iter()).rev() {
                    let ident = stack_idents.pop().unwrap();
                    writeln!(
                        self.w,
                        "    {} {} = {};",
                        Self::format_tid(tid),
                        Self::format_var(*var),
                        Self::format_stack_ident(ident)
                    )?;
                }
            }
            crate::typecheck::expr::ExprKind::Call => todo!(),
            crate::typecheck::expr::ExprKind::Type(tid) => {
                let ident = self.next_stack_ident();
                stack_idents.push(ident);
                writeln!(
                    self.w,
                    "    const {} {} = {};",
                    Self::format_metatype_tid(),
                    Self::format_stack_ident(ident),
                    Self::format_metatype(*tid)
                )?
            }
            crate::typecheck::expr::ExprKind::UncheckedCast => todo!(),
            crate::typecheck::expr::ExprKind::Add(type_id) => todo!(),
            crate::typecheck::expr::ExprKind::Pack => {
                let FunctionType { mut input, output } = self.get_stack_effect(tid);
                assert!(output.len() == 1);

                // discard the metatype
                stack_idents.pop();
                input.pop();

                match self.typestore.get(output[0]).unwrap().clone() {
                    Type::Struct(struct_type) => todo!(),
                    Type::Variant(variant) => {
                        assert!(input.len() <= 1);

                        let parent_tid = self.typestore.intern(Type::Enum(EnumType {
                            eid: variant.eid,
                            args: variant.args.clone(),
                        }));

                        let input_tid = input.pop();
                        let ident = self.next_stack_ident();
                        writeln!(
                            self.w,
                            "    const {} {} = {{",
                            Self::format_tid(output[0]),
                            Self::format_stack_ident(ident)
                        )?;
                        writeln!(
                            self.w,
                            "        .tag = {},",
                            Self::format_enum_tag(variant.eid, parent_tid, variant.variant)
                        )?;
                        if let Some(_) = input_tid {
                            let input_ident = stack_idents.pop().unwrap();
                            writeln!(
                                self.w,
                                "        .value._{} = {},",
                                variant.variant,
                                Self::format_stack_ident(input_ident)
                            )?;
                        }
                        writeln!(self.w, "    }};")
                    }
                    _ => unreachable!(),
                };
            }
            crate::typecheck::expr::ExprKind::NamedMemberAccess { var, member } => todo!(),
            crate::typecheck::expr::ExprKind::MemberAccess { var, member } => todo!(),
            crate::typecheck::expr::ExprKind::Match(match_expr) => {
                self.generate_match_expr(tid, &match_expr, stack_idents)?
            }
            crate::typecheck::expr::ExprKind::If(if_expr) => todo!(),
            crate::typecheck::expr::ExprKind::Read => todo!(),
        }

        Ok(())
    }

    fn next_stack_ident(&mut self) -> usize {
        let ident = self.ident_count;
        self.ident_count += 1;
        ident
    }

    fn generate_input_unbinding(
        &mut self,
        input: &[TypeId],
    ) -> Result<Vec<usize>, std::fmt::Error> {
        let mut stack = vec![];
        for (idx, tid) in input.iter().enumerate() {
            let stack_ident = self.next_stack_ident();
            writeln!(
                self.w,
                "    const {} {} = input._{idx};",
                Self::format_tid(*tid),
                Self::format_stack_ident(stack_ident)
            )?;
            stack.push(stack_ident)
        }

        Ok(stack)
    }

    fn declare_function(&mut self, var: &Var, func: &TypedFunction<'src>) -> std::fmt::Result {
        let tid = func.expr.meta.1;
        let Type::Function(FunctionType { input, output }) =
            self.typestore.get(tid).unwrap().clone()
        else {
            unreachable!();
        };

        writeln!(
            self.w,
            "{} {}({}{}) {{",
            Self::format_function_output(tid),
            Self::format_var(*var),
            Self::format_function_input(tid),
            match input.len() {
                0 => "",
                _ => " input",
            }
        )?;
        let mut stack = self.generate_input_unbinding(&input)?;
        self.generate_expr(&func.expr, &mut stack)?;
        writeln!(self.w, "}}");

        Ok(())
    }

    fn declare_functions(&mut self) -> std::fmt::Result {
        for (var, func) in self.fns {
            self.declare_function(var, func)?;
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
            ident_count: 0,
        };

        writeln!(&mut gen.w, "#include <stdint.h>")?;
        writeln!(&mut gen.w, "#include <stdbool.h>")?;
        gen.forward_declare_types()?;
        writeln!(&mut gen.w);
        gen.declare_types()?;
        writeln!(&mut gen.w);
        gen.forward_declare_functions()?;
        writeln!(&mut gen.w);
        gen.declare_functions()?;

        Ok(())
    }
}

enum VisitState {
    Visiting,
    Visited,
}
