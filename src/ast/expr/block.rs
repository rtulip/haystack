use std::collections::HashMap;

use crate::{
    ast::stmt::GlobalEnv,
    error::HayError,
    lex::token::Token,
    types::{Frame, Stack, TypeId, TypeMap, UncheckedFunction},
};

use super::{Expr, TypedExpr};

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub open: Token,
    pub close: Token,
    pub exprs: Vec<Expr>,
}

impl BlockExpr {
    pub fn type_check(
        self,
        stack: &mut Stack,
        frame: &mut Frame,
        func: &UncheckedFunction,
        global_env: &mut GlobalEnv,
        types: &mut TypeMap,
        generic_map: &Option<HashMap<TypeId, TypeId>>,
    ) -> Result<TypedExpr, HayError> {
        let saved_frame = frame.clone();
        let mut typed_exprs = vec![];
        for e in self.exprs {
            typed_exprs.push(e.type_check(stack, frame, func, global_env, types, generic_map)?);
        }

        *frame = saved_frame;
        Ok(TypedExpr::Block { exprs: typed_exprs })
    }
}
