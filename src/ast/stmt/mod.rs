mod r#enum;
mod function;
mod r#impl;
mod interface;
mod pre_decl;
mod record;
mod stmt_kind;
mod stmt_typ;
mod stub;
mod var;

use std::collections::HashMap;

pub use function::*;
pub use interface::*;
pub use pre_decl::*;
pub use r#enum::*;
pub use r#impl::*;
pub use record::*;
pub use stmt_kind::*;
pub use stmt_typ::*;
pub use stub::*;
pub use var::*;

use crate::types::Signature;

pub type GlobalEnv<'a> = HashMap<String, (StmtKind, Signature<'a>)>;
