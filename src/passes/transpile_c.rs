use crate::expr::{Expr, Literal};

use super::{Assignment, CSsaExtension};

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
                generate!(indentation, "{} = {b};", self.meta.output.as_ref().unwrap());
            },
            crate::expr::ExprBase::Literal(Literal::U32(n))  => {
                generate!(indentation, "{} = {n};", self.meta.output.as_ref().unwrap());
            },
            crate::expr::ExprBase::Literal(Literal::U8(n))  => {
                generate!(indentation, "{} = {n};", self.meta.output.as_ref().unwrap());
            },
            crate::expr::ExprBase::Literal(Literal::String(s)) => {
                generate!(indentation, "{} = {{ .size = {}, .string = \"{}\" }};", self.meta.output.as_ref().unwrap(), s.len(), s);
            },
            crate::expr::ExprBase::Print => {
                generate!(indentation, "printf(\"%u\\n\", {:?});", self.meta.input.as_ref().unwrap()[0]);
            },
            crate::expr::ExprBase::PrintString => {
                generate!(indentation, "printf(\"%s\\n\", {:?}.string);", self.meta.input.as_ref().unwrap()[0]);
            },
            crate::expr::ExprBase::Block(exprs) => {
                generate!(indentation, "{{");
                exprs.iter().for_each(|e| e.transpile(indentation + tab_size, tab_size));
                generate!(indentation, "}}");
            },
            crate::expr::ExprBase::Ext(_) => todo!(),
        }
    }

}