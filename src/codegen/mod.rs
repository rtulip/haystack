use std::{
    collections::{HashMap, HashSet},
    eprintln,
    fmt::Write,
    format, matches, todo, unreachable, vec, writeln,
};

use crate::{
    lex::token::Token,
    typecheck::{
        expr::{Expr, ExprKind, MatchCase, MatchExpr, MatchPattern, Var},
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
    externs: &'a HashMap<Var, (&'src str, TypeId)>,
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

    fn format_tuple_id(tid: TypeId) -> String {
        format!("haystack_tuple_{}", tid.0)
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
            Type::Tuple(_) => {
                writeln!(self.w, "struct {};", Self::format_tuple_id(*tid))?;
            }
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

    fn format_c_compat_type(&self, tid: TypeId) -> String {
        match self.typestore.get(tid).unwrap() {
            Type::Builtin(b) => match b {
                BuiltinType::U8 => format!("uint8_t"),
                BuiltinType::U64 => format!("uint64_t"),
                BuiltinType::Bool => format!("bool"),
                BuiltinType::CVarArgs => format!("..."),
            },
            Type::Function(function_type) => todo!(),
            Type::Pointer(p) => {
                let modifier = Self::format_mutability(p.mutability);
                format!("{modifier}{}*", self.format_c_compat_type(p.tid))
            }
            Type::Never => format!("void"),
            ty => unreachable!("{ty:?}"),
        }
    }

    fn format_c_compat_args(&self, tids: &[TypeId]) -> String {
        if tids.is_empty() {
            return String::new();
        }

        if tids.len() == 1 {
            return self.format_c_compat_type(tids[0]);
        }

        let mut output = self.format_c_compat_type(tids[0]);

        for tid in &tids[1..] {
            output = format!("{output}, {}", self.format_c_compat_type(*tid));
        }

        output
    }

    fn declare_extern_fn(&mut self, name: &str, tid: TypeId) -> std::fmt::Result {
        let FunctionType { input, output } = self.get_stack_effect(tid);

        assert!(output.len() == 1);

        writeln!(
            self.w,
            "extern {} {name}({});",
            Self::format_c_compat_type(&self, output[0]),
            Self::format_c_compat_args(&self, &input)
        )?;
        Ok(())
    }

    fn declare_externs(&mut self, extern_fns: &'a [(Var, &'src str, TypeId)]) -> std::fmt::Result {
        for (_, name, tid) in extern_fns {
            self.declare_extern_fn(name, *tid)?;
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
                    BuiltinType::CVarArgs => unreachable!(),
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
            Type::Tuple(inner) => {
                writeln!(self.w, "typedef struct {} {{", Self::format_tuple_id(tid))?;
                for (tag, tid) in inner.iter().enumerate() {
                    writeln!(self.w, "    {} _{tag};", Self::format_tid(*tid))?;
                }
                writeln!(self.w, "}} {};", Self::format_tid(tid));
            }
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
        if metatypes.is_empty() {
            return Ok(());
        }

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
        stack: &mut Vec<(TypeId, usize)>,
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
            let (_, ident) = stack.pop().unwrap();
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
        stack: &mut Vec<(TypeId, usize)>,
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
            stack.push((*tid, ident));
        }
        Ok(())
    }

    fn format_extern_input(&self, tid: TypeId, ident: &(TypeId, usize)) -> String {
        match self.typestore.get(tid).unwrap() {
            Type::Builtin(BuiltinType::CVarArgs) => match self.typestore.get(ident.0).unwrap() {
                Type::Tuple(tids) => {
                    if tids.is_empty() {
                        return String::new();
                    }

                    tids[1..].iter().enumerate().fold(
                        format!("{}._0", Self::format_stack_ident(ident.1)),
                        |acc, (idx, _)| {
                            format!("{acc}, {}._{}", Self::format_stack_ident(ident.1), idx + 1)
                        },
                    )
                }
                ty => todo!("{ty:?}"),
            },
            _ => Self::format_stack_ident(ident.1),
        }
    }

    fn format_extern_function_inputs(
        &self,
        input: Vec<TypeId>,
        stack: &mut Vec<(TypeId, usize)>,
    ) -> String {
        if input.is_empty() {
            return String::new();
        }

        assert!(input.len() == stack.len());
        let idents = stack.split_off(stack.len() - input.len());

        let start = self.format_extern_input(input[0], &idents[0]);

        input[1..]
            .into_iter()
            .zip(idents[1..].into_iter())
            .fold(start, |acc, (tid, ident)| {
                format!(
                    "{acc}, {}",
                    self.format_extern_input(*tid, ident)
                )
            })
    }

    fn generate_extern_function_call(
        &mut self,
        name: &str,
        tid: TypeId,
        stack: &mut Vec<(TypeId, usize)>,
    ) -> std::fmt::Result {
        let FunctionType { input, output } = self.get_stack_effect(tid);
        eprintln!(
            "{name}: {} (input : {}) (output: {})",
            self.typestore.fmt(tid),
            self.typestore.fmt_many(&input),
            self.typestore.fmt_many(&output)
        );

        assert!(output.len() == 1);

        let ident = self.next_stack_ident();
        let inputs = self.format_extern_function_inputs(input, stack);

        writeln!(
            self.w,
            "    {} {} = {name}({inputs});",
            Self::format_tid(output[0]),
            Self::format_stack_ident(ident),
        )?;

        stack.push((output[0], ident));

        Ok(())
    }

    fn generate_function_call(
        &mut self,
        tid: TypeId,
        var: &Var,
        stack: &mut Vec<(TypeId, usize)>,
    ) -> std::fmt::Result {
        if let Some((name, tid)) = self.externs.get(var) {
            return self.generate_extern_function_call(name, *tid, stack);
        }

        let func = self.get_stack_effect(tid);
        let input = self.generate_input_binding(tid, &func.input, stack)?;

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
            self.generate_output_unbinding(output, &func.output, stack)?;
        }
        Ok(())
    }

    fn generate_match_case(
        &mut self,
        input: usize,
        outvars: &[(TypeId, usize)],
        tid: TypeId,
        case: &MatchCase<'src, (Token<'src>, TypeId)>,
        first: bool,
        mut stack: Vec<(TypeId, usize)>,
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
                    stack.push((case_input[0], ident));

                    writeln!(
                        self.w,
                        "    const {} {} = {}.value._{};",
                        Self::format_tid(case_input[0]),
                        Self::format_stack_ident(ident),
                        Self::format_stack_ident(input),
                        variant.variant
                    )?;
                }

                self.generate_expr(&case.expr, &mut stack)?;

                for (_, outvar) in outvars.iter().rev() {
                    let (_, ident) = stack.pop().unwrap();
                    writeln!(
                        self.w,
                        "    {} = {};",
                        Self::format_stack_ident(*outvar),
                        Self::format_stack_ident(ident)
                    )?;
                }

                writeln!(self.w, "    }}")?;
            }
        }

        Ok(())
    }

    fn generate_match_expr(
        &mut self,
        tid: TypeId,
        match_expr: &MatchExpr<'src, (Token<'src>, TypeId)>,
        stack: &mut Vec<(TypeId, usize)>,
    ) -> std::fmt::Result {
        let (_, enum_input) = stack.pop().unwrap();

        let FunctionType { input, output } = self.get_stack_effect(tid);

        let mut outvars = vec![];
        for tid in &output {
            let ident = self.next_stack_ident();
            outvars.push((*tid, ident));
            writeln!(
                self.w,
                "    {} {};",
                Self::format_tid(*tid),
                Self::format_stack_ident(ident)
            )?;
        }
        assert!(input.len() == 1);

        let mut first = true;
        for case in &match_expr.cases {
            self.generate_match_case(
                enum_input,
                &outvars,
                input[0],
                &case,
                first,
                stack.clone(),
            )?;
            first = false;
        }
        if let Some(finally) = &match_expr.finally {
            todo!();
        }

        for output in outvars {
            stack.push(output);
        }

        Ok(())
    }

    fn generate_expr(
        &mut self,
        expr: &Expr<'src, (Token<'src>, TypeId)>,
        stack: &mut Vec<(TypeId, usize)>,
    ) -> std::fmt::Result {
        writeln!(self.w, "    // {}", expr.meta.0)?;
        let tid = expr.meta.1;
        match &expr.expr {
            ExprKind::U8(_) => todo!(),
            ExprKind::U64(_) => todo!(),
            ExprKind::Bool(b) => {
                let FunctionType { input, output } = self.get_stack_effect(tid);
                assert!(input.len() == 0);
                assert!(output.len() == 1);
                let ident = self.next_stack_ident();
                stack.push((output[0], ident));

                writeln!(
                    self.w,
                    "    const {} {} = {b};",
                    Self::format_tid(output[0]),
                    Self::format_stack_ident(ident)
                )?;
            }
            ExprKind::String(s) => {
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
                writeln!(self.w, "        ._1 = sizeof(\"{s}\") - 1,")?;
                writeln!(self.w, "    }};")?;

                stack.push((output[0], ident));
            }
            ExprKind::Ident(var) => match self.typestore.get(expr.meta.1).unwrap() {
                Type::Function(_) => {
                    eprintln!("{}", &expr.meta.0);
                    self.generate_function_call(tid, var, stack)?;
                }
                _ => {
                    let ident = self.next_stack_ident();
                    stack.push((tid, ident));
                    writeln!(
                        self.w,
                        "    const {} {} = {};",
                        Self::format_tid(tid),
                        Self::format_stack_ident(ident),
                        Self::format_var(*var)
                    );
                }
            },
            ExprKind::Ref { var, mutability } => {
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

                stack.push((output[0], ident));
            }
            ExprKind::Block(exprs) => {
                let FunctionType { input, output } = self.get_stack_effect(tid);

                let mut idents = vec![];
                for tid in &output {
                    let ident = self.next_stack_ident();
                    idents.push((*tid, ident));
                    writeln!(
                        self.w,
                        "    {} {};",
                        Self::format_tid(*tid),
                        Self::format_stack_ident(ident)
                    );
                }

                writeln!(self.w, "    {{")?;
                for expr in exprs {
                    self.generate_expr(expr, stack)?;
                }

                for (_, outvar) in idents.iter().rev() {
                    let (_, invar) = stack.pop().unwrap();
                    writeln!(
                        self.w,
                        "    {} = {};",
                        Self::format_stack_ident(*outvar),
                        Self::format_stack_ident(invar)
                    )?;
                }

                stack.extend(idents);

                writeln!(self.w, "    }}")?;
            }
            ExprKind::Tuple(exprs) => {
                let FunctionType { input, output } = self.get_stack_effect(tid);

                assert!(input.len() == 0);
                assert!(output.len() == 1);

                let output_ident = self.next_stack_ident();
                writeln!(
                    self.w,
                    "    {} {};",
                    Self::format_tid(output[0]),
                    Self::format_stack_ident(output_ident)
                );
                writeln!(self.w, "    {{")?;
                let mut fresh_stack = vec![];
                for expr in exprs {
                    self.generate_expr(expr, &mut fresh_stack)?;
                }

                writeln!(
                    self.w,
                    "    {} = ({}){{",
                    Self::format_stack_ident(output_ident),
                    Self::format_tid(output[0]),
                )?;
                for (_, ident) in fresh_stack {
                    writeln!(self.w, "        {},", Self::format_stack_ident(ident))?;
                }
                writeln!(self.w, "    }};")?;
                writeln!(self.w, "    }}")?;

                stack.push((output[0], output_ident));
            }
            ExprKind::Bind(vars) => {
                let FunctionType { input, output } = self.get_stack_effect(tid);
                assert!(input.len() == vars.len());
                assert!(output.len() == 0);
                assert!(vars.len() <= stack.len());

                for (tid, var) in input.into_iter().zip(vars.iter()).rev() {
                    let (_, ident) = stack.pop().unwrap();
                    writeln!(
                        self.w,
                        "    {} {} = {};",
                        Self::format_tid(tid),
                        Self::format_var(*var),
                        Self::format_stack_ident(ident)
                    )?;
                }
            }
            ExprKind::Call => todo!(),
            ExprKind::Type(tid) => {
                let ident = self.next_stack_ident();
                // TODO: metatypes can't be handled quite like normal tids.
                stack.push((expr.meta.1, ident));
                writeln!(
                    self.w,
                    "    const {} {} = {};",
                    Self::format_metatype_tid(),
                    Self::format_stack_ident(ident),
                    Self::format_metatype(*tid)
                )?
            }
            ExprKind::UncheckedCast => todo!(),
            ExprKind::Add(type_id) => todo!(),
            ExprKind::Pack => {
                let FunctionType { mut input, output } = self.get_stack_effect(tid);
                assert!(output.len() == 1);

                // discard the metatype
                stack.pop();
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
                            let (_, input_ident) = stack.pop().unwrap();
                            writeln!(
                                self.w,
                                "        .value._{} = {},",
                                variant.variant,
                                Self::format_stack_ident(input_ident)
                            )?;
                        }
                        writeln!(self.w, "    }};")?;

                        stack.push((output[0], ident));
                    }
                    _ => unreachable!(),
                };
            }
            ExprKind::NamedMemberAccess { var, member } => todo!(),
            ExprKind::MemberAccess { var, member } => {
                let FunctionType { input, output } = self.get_stack_effect(tid);
                assert!(input.len() == 0);
                assert!(output.len() == 1);

                let ident = self.next_stack_ident();
                stack.push((output[0], ident));
                writeln!(
                    self.w,
                    "    {} {} = {}._{};",
                    Self::format_tid(output[0]),
                    Self::format_stack_ident(ident),
                    Self::format_var(*var),
                    *member
                );
            }
            ExprKind::Match(match_expr) => {
                self.generate_match_expr(tid, &match_expr, stack)?
            }
            ExprKind::If(if_expr) => todo!(),
            ExprKind::Read => todo!(),
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
    ) -> Result<Vec<(TypeId, usize)>, std::fmt::Error> {
        let mut stack = vec![];
        for (idx, tid) in input.iter().enumerate() {
            let stack_ident = self.next_stack_ident();
            writeln!(
                self.w,
                "    const {} {} = input._{idx};",
                Self::format_tid(*tid),
                Self::format_stack_ident(stack_ident)
            )?;
            stack.push((*tid, stack_ident))
        }

        Ok(stack)
    }

    fn generate_return(
        &mut self,
        tid: TypeId,
        output: &[TypeId],
        stack: &mut Vec<(TypeId, usize)>,
    ) -> std::fmt::Result {
        if output.len() == 0 {
            return Ok(());
        }

        assert!(output.len() == stack.len());
        writeln!(
            self.w,
            "    const {} output = {{",
            Self::format_function_output(tid)
        )?;
        for (idx, (_, ident)) in stack.iter().enumerate() {
            writeln!(
                self.w,
                "        ._{idx} = {},",
                Self::format_stack_ident(*ident)
            )?;
        }
        writeln!(self.w, "    }};")?;

        writeln!(self.w, "    return output;")?;

        Ok(())
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
        self.generate_return(tid, &output, &mut stack)?;
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

    pub fn generate_entry(&mut self, var: Var) -> std::fmt::Result {
        writeln!(self.w, "int main() {{")?;
        writeln!(self.w, "    {}();", Self::format_var(var))?;
        writeln!(self.w, "}}")
    }

    pub fn generate(
        w: &'a mut W,
        typestore: &'a mut TypeStore<Token<'src>>,
        types: &'a Vec<TypeId>,
        fns: &'a [(Var, TypedFunction<'src>)],
        extern_fns: &'a [(Var, &'src str, TypeId)],
        entry: Var,
    ) -> std::fmt::Result {
        let types = Self::order_types(typestore, types);
        for tid in &types {
            eprintln!("{tid:?}");
        }
        let mut gen = CCodeGen {
            typestore,
            types: &types,
            fns,
            externs: &HashMap::from_iter(
                extern_fns
                    .iter()
                    .map(|(var, name, tid)| (*var, (*name, *tid))),
            ),
            w,
            ident_count: 0,
        };

        writeln!(&mut gen.w, "#include <stdint.h>")?;
        writeln!(&mut gen.w, "#include <stdbool.h>")?;
        gen.declare_externs(extern_fns)?;
        gen.forward_declare_types()?;
        writeln!(&mut gen.w);
        gen.declare_types()?;
        writeln!(&mut gen.w);
        gen.forward_declare_functions()?;
        writeln!(&mut gen.w);
        gen.declare_functions()?;
        writeln!(&mut gen.w);
        gen.generate_entry(entry)?;

        eprintln!("{}", typestore.fmt(TypeId(3)));

        Ok(())
    }
}

enum VisitState {
    Visiting,
    Visited,
}
