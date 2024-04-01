use std::collections::HashMap;

use crate::{
    c::{CFunction, CType, CVar},
    expr::{Expr, Literal},
    stmt::Function,
};

use super::{Assignment, CSsaExtension};

#[macro_export]
macro_rules! generate {
    ($indent:expr, $($arg:tt)*) => {{
        print!("{:width$}", "", width = $indent);
        println!($($arg)*)
    }};
}

impl<'src> Expr<'src, Assignment, CSsaExtension<'src>> {
    pub fn transpile(&self, indentation: usize, tab_size: usize) {
        match &self.expr {
            crate::expr::ExprBase::Literal(Literal::Bool(b)) => {
                generate!(
                    indentation,
                    "{:?} = {b};",
                    self.meta.output.as_ref().unwrap()[0]
                )
            }
            crate::expr::ExprBase::Literal(Literal::U32(n)) => {
                generate!(
                    indentation,
                    "{:?} = {n};",
                    self.meta.output.as_ref().unwrap()[0]
                )
            }
            crate::expr::ExprBase::Literal(Literal::U8(n)) => {
                generate!(
                    indentation,
                    "{:?} = {n};",
                    self.meta.output.as_ref().unwrap()[0]
                )
            }
            crate::expr::ExprBase::Literal(Literal::String(s)) => {
                generate!(
                    indentation,
                    "{:?} = {{ .size = {}, .data = (uint8_t*)\"{}\" }};",
                    self.meta.output.as_ref().unwrap()[0],
                    s.len(),
                    s
                )
            }
            crate::expr::ExprBase::Block(exprs) => {
                match &self.meta.output {
                    Some(output) => output
                        .iter()
                        .for_each(|var| generate!(indentation, "{var:?};")),
                    None => (),
                }

                generate!(indentation, "{{");
                exprs
                    .iter()
                    .for_each(|e| e.transpile(indentation + tab_size, tab_size));
                generate!(indentation, "}}")
            }
            crate::expr::ExprBase::BinOp(op) => {
                generate!(
                    indentation,
                    "{:?} = {} {op} {};",
                    self.meta.output.as_ref().unwrap()[0],
                    self.meta.input.as_ref().unwrap()[0],
                    self.meta.input.as_ref().unwrap()[1]
                )
            }
            crate::expr::ExprBase::Call(_) => {
                unreachable!("base call expressions should have been removed")
            }
            crate::expr::ExprBase::Ext(CSsaExtension::BackAssign { input, output }) => {
                generate!(indentation, "{output} = {input};")
            }
            crate::expr::ExprBase::Ext(CSsaExtension::Return(ty)) => {
                match self.meta.input.as_ref().unwrap().len() {
                    0 => unreachable!("Shouldn't be returning anything in this case"),
                    1 => {
                        generate!(
                            indentation,
                            "return {};",
                            self.meta.input.as_ref().unwrap()[0]
                        )
                    }
                    _ => {
                        generate!(indentation, "return ({ty}) {{");
                        self.meta
                            .input
                            .as_ref()
                            .unwrap()
                            .iter()
                            .enumerate()
                            .for_each(|(i, var)| {
                                generate!(indentation + tab_size, ".member{i} = {var},")
                            });
                        generate!(indentation, "}};");
                    }
                }
            }
            crate::expr::ExprBase::Ext(CSsaExtension::Call(name)) => {
                if self.meta.output.is_none() {
                    generate!(indentation, "{name}(");
                    self.meta
                        .input
                        .as_ref()
                        .unwrap()
                        .iter()
                        .enumerate()
                        .for_each(|(i, input)| {
                            generate!(
                                indentation + tab_size,
                                "{input}{}",
                                if i != self.meta.input.as_ref().unwrap().len() - 1 {
                                    ","
                                } else {
                                    ""
                                }
                            )
                        });
                    generate!(indentation, ");");
                } else {
                    generate!(
                        indentation,
                        "{:?} = {name}(",
                        self.meta.output.as_ref().unwrap()[0]
                    );
                    self.meta
                        .input
                        .as_ref()
                        .unwrap()
                        .iter()
                        .enumerate()
                        .for_each(|(i, input)| {
                            generate!(
                                indentation + tab_size,
                                "{input}{}",
                                if i != self.meta.input.as_ref().unwrap().len() - 1 {
                                    ","
                                } else {
                                    ""
                                }
                            )
                        });
                    generate!(indentation, ");");
                }
            }
            crate::expr::ExprBase::If { then, otherwise } => {
                match &self.meta.output {
                    Some(output) => output
                        .iter()
                        .for_each(|var| generate!(indentation, "{var:?};")),
                    None => (),
                }

                generate!(indentation, "if ({})", self.meta.input.as_ref().unwrap()[0]);
                then.transpile(indentation, tab_size);
                generate!(indentation, "else");
                otherwise.transpile(indentation, tab_size);
            }
            crate::expr::ExprBase::Bind(_) => (),
            crate::expr::ExprBase::Var(_) => {
                generate!(
                    indentation,
                    "{:?} = {};",
                    self.meta.output.as_ref().unwrap()[0],
                    self.meta.input.as_ref().unwrap()[0]
                );
            }
            crate::expr::ExprBase::VarAccessor(_, accessors) => {
                generate!(
                    indentation,
                    "{:?} = {}",
                    self.meta.output.as_ref().unwrap()[0],
                    self.meta.input.as_ref().unwrap()[0]
                );

                accessors
                    .iter()
                    .for_each(|inner| generate!(indentation + tab_size, ".{inner}"));
                generate!(indentation + tab_size, ";");
            }
            crate::expr::ExprBase::While { condition, body } => {
                generate!(indentation, "while (1) {{");
                condition
                    .iter()
                    .for_each(|e| e.transpile(indentation + tab_size, tab_size));
                body.transpile(indentation + tab_size, tab_size);
                generate!(indentation, "}}");
            }
            crate::expr::ExprBase::ExternCall(id) => todo!(),
            crate::expr::ExprBase::Ext(CSsaExtension::ExitLoop) => {
                generate!(
                    indentation,
                    "if (!{}) break;",
                    self.meta.input.as_ref().unwrap()[0]
                )
            }
        }
    }

    pub fn get_ctypes(&self) -> HashMap<String, CType> {
        let mut types = HashMap::new();

        self.for_each_meta(|a| {
            match &a.input {
                Some(input) => input.iter().for_each(|var| {
                    let t = &var.ty;
                    let k = format!("{t}");
                    if !types.contains_key(&k) {
                        types.insert(k, t.clone());
                    }
                }),
                None => (),
            };

            match &a.output {
                Some(single) if single.len() == 1 => {
                    let var = &single[0];
                    let t = &var.ty;
                    let k = format!("{t}");
                    if !types.contains_key(&k) {
                        types.insert(k, t.clone());
                    }
                }
                Some(vars) => {
                    let ty =
                        CType::tuple(vars.iter().map(|var| var.ty.clone()).collect::<Vec<_>>());
                    let k = format!("{ty}");
                    if !types.contains_key(&k) {
                        types.insert(k, ty);
                    }
                }
                None => (),
            }
        });

        types
    }
}

impl<'src> Function<'src, Assignment, CSsaExtension<'src>> {
    fn signature(&self, indentation: usize, tab_size: usize) {
        let ty = CType::from(
            self.output
                .iter()
                .map(|ty| CType::from(ty.clone()))
                .collect::<Vec<_>>(),
        );

        generate!(indentation, "{ty} {}(", self.name);
        CVar::from_stack(self.input.clone())
            .0
            .iter()
            .enumerate()
            .for_each(|(i, var)| {
                generate!(
                    indentation + tab_size,
                    "{var:?}{}",
                    if i != self.input.len() - 1 { "," } else { "" }
                )
            });
        generate!(indentation, ")");
    }

    pub fn declare(&self, indentation: usize, tab_size: usize) {
        self.signature(indentation, tab_size);
        generate!(indentation, ";");
        generate!(indentation, "");
    }

    pub fn transpile(&self, indentation: usize, tab_size: usize) {
        self.signature(indentation, tab_size);

        generate!(indentation, "{{");
        self.body.transpile(indentation + tab_size, tab_size);
        generate!(indentation, "}}");
        generate!(indentation, "");
    }
}

impl<'src> CType {
    pub fn transpile(&self, indentation: usize, tab_size: usize) {
        match self {
            CType::Struct { name, elements } => {
                generate!(indentation, "typedef struct {name} {{");
                elements
                    .iter()
                    .for_each(|(ident, ty)| generate!(indentation + tab_size, "{ty} {ident};"));
                generate!(indentation, "}} {name};");
                generate!(indentation, "");
            }
            CType::Tuple(elements) => {
                generate!(indentation, "typedef struct {self} {{");
                elements
                    .iter()
                    .enumerate()
                    .for_each(|(i, ty)| generate!(indentation + tab_size, "{ty} member{i};"));
                generate!(indentation, "}} {self};");
                generate!(indentation, "");
            }
            CType::U32
            | CType::Bool
            | CType::U8
            | CType::Char
            | CType::Pointer(_)
            | CType::Void
            | CType::Int
            | CType::Variadic => (),
            CType::Function(CFunction {
                name,
                input,
                output,
            }) => {
                generate!(indentation, "{output} {name}(");

                input.iter().enumerate().for_each(|(i, var)| {
                    generate!(
                        indentation + tab_size,
                        "{var}{}",
                        if i != input.len() - 1 { "," } else { "" }
                    )
                });

                generate!(indentation, ");")
            }
        }
    }

    pub fn declare(&self, indentation: usize) {
        match self {
            CType::Struct { name, .. } => {
                generate!(indentation, "struct {name};")
            }
            CType::Tuple(_) => {
                generate!(indentation, "struct {self};")
            }
            CType::U32
            | CType::Bool
            | CType::U8
            | CType::Char
            | CType::Pointer(_)
            | CType::Void
            | CType::Variadic
            | CType::Int => (),
            CType::Function { .. } => todo!(),
        }
    }
}
