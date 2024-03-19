use crate::expr::{Expr, Literal};

use super::{Assignment, CSsaExtension, CType};

#[macro_export]
macro_rules! generate {
    ($indent:expr, $($tts:expr),*) => {
        print!("{:width$}", "", width = $indent);
        println!($($tts),*)
    };
}

impl<'src> Expr<'src, Assignment<'src>, CSsaExtension<'src>> {
    pub fn transpile(&self, indentation: usize, tab_size: usize) {
        match &self.expr {
            crate::expr::ExprBase::Literal(Literal::Bool(b)) => {
                generate!(
                    indentation,
                    "{} = {b};",
                    self.meta.output.as_ref().unwrap()[0]
                );
            }
            crate::expr::ExprBase::Literal(Literal::U32(n)) => {
                generate!(
                    indentation,
                    "{} = {n};",
                    self.meta.output.as_ref().unwrap()[0]
                );
            }
            crate::expr::ExprBase::Literal(Literal::U8(n)) => {
                generate!(
                    indentation,
                    "{} = {n};",
                    self.meta.output.as_ref().unwrap()[0]
                );
            }
            crate::expr::ExprBase::Literal(Literal::String(s)) => {
                generate!(
                    indentation,
                    "{} = {{ .size = {}, .string = \"{}\" }};",
                    self.meta.output.as_ref().unwrap()[0],
                    s.len(),
                    s
                );
            }
            crate::expr::ExprBase::Print => {
                generate!(
                    indentation,
                    "printf(\"%u\\n\", {:?});",
                    self.meta.input.as_ref().unwrap()[0]
                );
            }
            crate::expr::ExprBase::PrintString => {
                generate!(
                    indentation,
                    "printf(\"%.*s\\n\", {:?}.size, {:?}.string);",
                    self.meta.input.as_ref().unwrap()[0],
                    self.meta.input.as_ref().unwrap()[0]
                );
            }
            crate::expr::ExprBase::Block(exprs) => {
                match &self.meta.output {
                    Some(output) => output.iter().for_each(|var| {
                        generate!(indentation, "{var};");
                    }),
                    None => (),
                }

                generate!(indentation, "{{");
                exprs
                    .iter()
                    .for_each(|e| e.transpile(indentation + tab_size, tab_size));
                generate!(indentation, "}}");
            }
            crate::expr::ExprBase::Ext(CSsaExtension::BackAssign { input, output }) => {
                generate!(indentation, "{output:?} = {input:?};");
            }
            crate::expr::ExprBase::Ext(_) => todo!(),
        }
    }
}

impl<'src> CType<'src> {
    pub fn transpile(&self, indentation: usize, tab_size: usize) {
        match self {
            CType::Struct { name, elements } => {
                generate!(indentation, "typedef struct {name} {{");
                elements.iter().for_each(|(ident, ty)| {
                    generate!(indentation + tab_size, "{ty} {ident};");
                });
                generate!(indentation, "}} {name};");
                generate!(indentation, "");
            }
            CType::Tuple(_)
            | CType::U32
            | CType::Bool
            | CType::U8
            | CType::Char
            | CType::Pointer(_) => (),
        }
    }
}
