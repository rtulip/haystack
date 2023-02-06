use std::collections::HashMap;

use crate::{
    ast::expr::ExprVar,
    backend::{InitData, UninitData},
    error::HayError,
    lex::token::Token,
    types::{Signature, TypeId, TypeMap},
};

use super::StmtKind;

#[derive(Debug, Clone)]
pub struct VarStmt {
    pub token: Token,
    pub expr: ExprVar,
}

impl VarStmt {
    pub fn add_to_global_scope(
        self,
        types: &mut TypeMap,
        global_env: &mut HashMap<String, (StmtKind, Signature)>,
        init_data: &mut HashMap<String, InitData>,
        uninit_data: &mut HashMap<String, UninitData>,
    ) -> Result<(), HayError> {
        let inner = TypeId::from_token(&self.expr.typ, types, &vec![])?;
        let inner_size = inner.size(types)?;
        let sig = Signature::new(vec![], vec![inner.ptr_of(true, types)]);

        if let Some((dimension, tt)) = self.expr.typ.dimension()? {
            let inner_typ = TypeId::from_type_token(&self.expr.typ, &tt, types, &vec![])?;
            let inner_size = inner_typ.size(types)?;

            let data_id = uninit_data.len();
            uninit_data.insert(
                format!("data_{data_id}"),
                UninitData::Region(inner_size * dimension),
            );
            init_data.insert(
                self.expr.ident.lexeme.clone(),
                InitData::Arr {
                    size: dimension,
                    pointer: format!("data_{data_id}"),
                },
            );
        } else {
            uninit_data.insert(
                self.expr.ident.lexeme.clone(),
                UninitData::Region(inner_size),
            );
        }

        match global_env.insert(self.expr.ident.lexeme.clone(), (StmtKind::Var, sig)) {
            None => Ok(()),
            Some(_) => Err(HayError::new(
                format!(
                    "Var conflict. `{}` defined elsewhere",
                    self.expr.ident.lexeme
                ),
                self.token.loc,
            )),
        }
    }
}
