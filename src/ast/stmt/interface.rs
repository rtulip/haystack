use crate::{
    ast::arg::UntypedArg,
    error::HayError,
    lex::token::Token,
    types::{FreeVars, Stack, Substitutions, Type, TypeId, TypeVar},
};

use super::{
    FunctionStmt, FunctionStubStmt, Functions, InterfaceFunctionTable, Interfaces, StmtKind,
    UserDefinedTypes,
};

#[derive(Debug, Clone)]
pub struct InterfaceId(String);

impl InterfaceId {
    pub fn new<S: Into<String>>(s: S) -> Self {
        InterfaceId(s.into())
    }
}

#[derive(Clone)]
pub struct InterfaceStmt {
    pub token: Token,
    pub name: Token,
    pub annotations: Vec<UntypedArg>,
    pub types: Vec<(TypeId, Token)>,
    pub stubs: Vec<FunctionStubStmt>,
    pub fns: Vec<FunctionStmt>,
    pub requires: Option<Vec<Token>>,
}

pub struct InterfaceDescription {
    token: Token,
    ordered_free_vars: Vec<TypeVar>,
    functions: Functions,
    impls: Vec<InterfaceImpl>,
}

pub struct InterfaceImpl {
    functions: Functions,
}

impl InterfaceStmt {
    pub fn add_to_global_env(
        self,
        user_defined_types: &UserDefinedTypes,
        interfaces: &mut Interfaces,
        interface_fn_table: &mut InterfaceFunctionTable,
    ) -> Result<(), HayError> {
        let interface_id = self.name.lexeme.clone();

        let (free_vars, ordered_free_vars) = UntypedArg::into_free_vars(Some(self.annotations));
        let associated_typs =
            FreeVars::from_iter(self.types.into_iter().map(|(t, _)| TypeVar::new(t.0)));

        let mut functions = Functions::new();

        if let Some(_) = &self.requires {
            todo!();
        }

        for func in self.fns {
            interface_fn_table.insert(func.name.lexeme.clone(), interface_id.clone());
            func.add_to_global_env(
                user_defined_types,
                &mut functions,
                Type::merge_free_vars(free_vars.as_ref(), Some(&associated_typs)).as_ref(),
            )?;
        }

        for stub in self.stubs {
            interface_fn_table.insert(stub.name.lexeme.clone(), interface_id.clone());
            stub.add_to_global_env(
                user_defined_types,
                &mut functions,
                Type::merge_free_vars(free_vars.as_ref(), Some(&associated_typs)).as_ref(),
            )?;
        }

        let interface = InterfaceDescription {
            token: self.name,
            ordered_free_vars: ordered_free_vars.unwrap(),
            functions,
            impls: vec![],
        };

        if let Some(prev) = interfaces.insert(interface_id, interface) {
            todo!();
        }

        Ok(())
    }
}

impl InterfaceDescription {
    pub fn unify(
        &self,
        func: &String,
        token: &Token,
        stack: &mut Stack,
        subs: &mut Substitutions,
    ) -> Result<(), HayError> {
        for iface_impl in &self.impls {
            let f = iface_impl.functions.get(func).unwrap();
            let stack_before = stack.clone();
            let subs_before = subs.clone();
            if f.typ.unify(token, stack, subs).is_err() {
                *stack = stack_before;
                *subs = subs_before;
                continue;
            }

            return Ok(());
        }

        todo!()
    }
}
