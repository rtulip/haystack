use crate::{
    ast::{arg::UntypedArg, expr::Expr},
    error::HayError,
    lex::token::Token,
    types::{FnTag, GenericFunction, Signature, Type, TypeId, TypeMap, UncheckedFunction},
};

use super::{GlobalEnv, Stmt, StmtKind};

#[derive(Debug, Clone)]
pub struct FunctionStmt {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<UntypedArg>,
    pub outputs: Vec<UntypedArg>,
    pub annotations: Option<Vec<UntypedArg>>,
    pub body: Vec<Expr>,
    pub tags: Vec<FnTag>,
    pub impl_on: Option<Token>,
}

impl FunctionStmt {
    pub fn add_to_global_scope(
        self,
        types: &mut TypeMap,
        global_env: &mut GlobalEnv,
        local_scope: Option<&TypeId>,
        kind: StmtKind,
    ) -> Result<(), HayError> {
        let generics = Stmt::bulid_local_generics(self.annotations, types, local_scope)?;
        let inputs = UntypedArg::resolve(self.inputs, types, &generics)?;
        let outputs = UntypedArg::resolve(self.outputs, types, &generics)?;

        let sig = Signature::new_maybe_generic(
            inputs.iter().map(|arg| arg.typ.clone()).collect(),
            outputs.iter().map(|arg| arg.typ.clone()).collect(),
            if generics.is_empty() {
                None
            } else {
                Some(generics.clone())
            },
        );

        let impl_on = match self.impl_on {
            Some(tok) => {
                if !types.contains_key(&TypeId::new(&tok.lexeme)) {
                    panic!("Logic error. Unknown type: {tok}");
                }

                Some(TypeId::new(&tok.lexeme))
            }
            None => None,
        };

        let typ = if generics.is_empty() {
            Type::UncheckedFunction {
                func: UncheckedFunction {
                    token: self.token,
                    name: self.name.clone(),
                    inputs,
                    outputs,
                    body: self.body,
                    generic_map: None,
                    tags: self.tags,
                    impl_on,
                },
            }
        } else {
            Type::GenericFunction {
                func: GenericFunction {
                    token: self.token,
                    name: self.name.clone(),
                    inputs,
                    outputs,
                    generics,
                    body: self.body,
                    tags: self.tags,
                    impl_on,
                },
            }
        };

        match types.insert(TypeId::new(&self.name.lexeme), typ) {
            None => {
                global_env.insert(self.name.lexeme, (kind, sig));

                Ok(())
            }
            Some(_) => {
                return Err(HayError::new(
                    format!(
                        "Function name conflict. `{}` defined elsewhere",
                        self.name.lexeme
                    ),
                    self.name.loc,
                ));
            }
        }
    }
}