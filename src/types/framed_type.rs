use std::fmt::Display;

use crate::lex::token::Token;

use super::TypeId;

#[derive(Debug, Clone)]
pub struct FramedType {
    pub origin: Token,
    pub typ: TypeId,
    pub mutable: bool,
}

impl FramedType {
    pub fn frame_to_string(frame: &Vec<(String, FramedType)>) -> String {
        let mut s = String::from("[");
        if !frame.is_empty() {
            for (ident, FramedType { typ, .. }) in &frame[0..frame.len() - 1] {
                s = format!("{s}{ident}: {typ} ");
            }
        }

        match frame.last() {
            Some((ident, FramedType { typ, .. })) => s = format!("{s}{ident}: {typ}]"),
            None => s.push(']'),
        }

        s
    }
}

impl Display for FramedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.typ)
    }
}

impl PartialEq for FramedType {
    fn eq(&self, other: &Self) -> bool {
        self.typ == other.typ && self.mutable == other.mutable
    }
}
