use crate::{
    ast::{arg::UntypedArg, expr::Expr},
    error::HayError,
    lex::token::Token,
    types::{
        validate_requirements, FnTag, GenericFunction, RecordKind, Signature, Type, TypeId,
        TypeMap, UncheckedFunction, VariantType,
    },
};

use super::{GlobalEnv, Stmt, StmtKind};

#[derive(Debug, Clone)]
pub struct FunctionStmt {
    pub token: Token,
    pub name: Token,
    pub inputs: Vec<UntypedArg>,
    pub outputs: Vec<UntypedArg>,
    pub annotations: Option<Vec<UntypedArg>>,
    pub body: Expr,
    pub tags: Vec<FnTag>,
    pub impl_on: Option<Token>,
    pub requires: Option<Vec<Token>>,
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

        for arg in inputs.iter().chain(outputs.iter()) {
            if let Some(Type::Variant(VariantType { base, variant })) = types.get(&arg.typ) {
                if let Some(Type::GenericRecordBase {
                    kind: RecordKind::EnumStruct,
                    ..
                }) = types.get(base)
                {
                    return Err(HayError::new(
                        format!("Enum struct `{base}` is generic, and requires annotations."),
                        arg.token.loc.clone(),
                    )
                    .with_hint(format!("Consider using `{base}<...>::{variant}`.")));
                }
            }
        }

        for arg in inputs.iter() {
            if let Some(Type::Never) = types.get(&arg.typ) {
                return Err(HayError::new(
                    format!(
                        "The Never type `!` isn't a valid input to function `{}`",
                        self.name.lexeme
                    ),
                    arg.token.loc.clone(),
                ));
            }
        }

        if let Some(requirements) = &self.requires {
            validate_requirements(requirements, types)?;
        }

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
            if self.requires.is_some() {
                return Err(HayError::new(
                    "Cannot have interface requirements on non-generic functions",
                    self.requires.unwrap().first().unwrap().loc.clone(),
                ));
            }

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
                    requires: self.requires,
                },
            }
        };

        match types.insert(TypeId::new(&self.name.lexeme), typ) {
            None => {
                global_env.insert(self.name.lexeme, (kind, sig));

                Ok(())
            }
            Some(_) => Err(HayError::new(
                format!(
                    "Function name conflict. `{}` defined elsewhere",
                    self.name.lexeme
                ),
                self.name.loc,
            )),
        }
    }
}
