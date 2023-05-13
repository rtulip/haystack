use std::{collections::HashSet, ops::Sub};

use crate::{
    ast::{arg::UntypedArg, expr::TypedExpr, stmt::FunctionDescription},
    error::HayError,
    lex::token::Token,
    types::{FreeVars, FunctionType, InterfaceType, Stack, Substitutions, Type, TypeId, TypeVar},
};

use super::{
    FunctionStmt, FunctionStubStmt, Functions, GlobalVars, InterfaceFunctionTable, Interfaces,
    StmtKind, UserDefinedTypes,
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

#[derive(Debug, Clone)]
pub struct InterfaceDescription {
    pub token: Token,
    pub ordered_free_vars: Vec<TypeVar>,
    pub free_vars: FreeVars,
    pub associated_types: HashSet<TypeVar>,
    pub functions: Functions,
    pub impls: Vec<InterfaceImpl>,
    pub requires: Option<Vec<Token>>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct InterfaceImpl {
    pub token: Token,
    pub subs: Substitutions,
    pub functions: Functions,
    pub requires: Option<Vec<Token>>,
    pub free_vars: Option<FreeVars>,
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
        let associated_types =
            FreeVars::from_iter(self.types.into_iter().map(|(t, _)| TypeVar::new(t.0)));

        let mut functions = Functions::new();

        if let Some(_) = &self.requires {
            todo!();
        }

        let free_vars = Type::merge_free_vars(free_vars.as_ref(), Some(&associated_types)).unwrap();

        for func in self.fns {
            interface_fn_table.insert(func.name.lexeme.clone(), interface_id.clone());
            func.add_to_global_env(
                user_defined_types,
                interfaces,
                &mut functions,
                Some(&free_vars),
            )?;
        }

        for stub in self.stubs {
            interface_fn_table.insert(stub.name.lexeme.clone(), interface_id.clone());
            stub.add_to_global_env(
                user_defined_types,
                interfaces,
                &mut functions,
                Some(&free_vars),
            )?;
        }

        let typ = Type::Interface(InterfaceType {
            iface: self.name.lexeme.clone(),
            types: ordered_free_vars
                .clone()
                .unwrap()
                .into_iter()
                .map(|t| Type::TypeVar(t))
                .collect(),
        });

        let interface = InterfaceDescription {
            token: self.name,
            ordered_free_vars: ordered_free_vars.unwrap(),
            free_vars,
            functions,
            associated_types,
            impls: vec![],
            requires: self.requires,
            typ,
        };

        if let Some(prev) = interfaces.insert(interface_id, interface) {
            todo!();
        }

        Ok(())
    }
}

impl InterfaceDescription {
    pub fn new_substitutions(
        &self,
        token: &Token,
        types: Vec<Type>,
    ) -> Result<Substitutions, HayError> {
        Substitutions::new(token, self.ordered_free_vars.clone(), types)
    }

    pub fn add_generic_impl(&mut self, token: &Token, subs: Substitutions) -> Result<(), HayError> {
        let mut functions = self.functions.clone();
        for (_, f) in &mut functions {
            f.typ = f.typ.substitute(&token, &subs)?;
        }

        self.impls.push(InterfaceImpl {
            token: self.token.clone(),
            subs,
            functions,
            requires: None,
            free_vars: None,
        });

        Ok(())
    }

    pub fn unify(
        &self,
        token: &Token,
        stack: &mut Stack,
        user_defined_types: &UserDefinedTypes,
        free_vars: Option<&FreeVars>,
        interfaces: &Interfaces,
        func: &String,
    ) -> Result<(usize, Substitutions), HayError> {
        for (idx, iface_impl) in self.impls.iter().enumerate() {
            let f = iface_impl.functions.get(func).unwrap();
            let stack_before = stack.clone();

            match f.typ.unify(token, stack) {
                Ok(subs) => {
                    match iface_impl.check_requirements(
                        token,
                        user_defined_types,
                        iface_impl.free_vars.as_ref(),
                        interfaces,
                        &subs,
                    ) {
                        Ok(_) => return Ok((idx, subs)),
                        Err(_) => {
                            *stack = stack_before;
                            continue;
                        }
                    }
                }
                Err(_) => {
                    *stack = stack_before;
                    continue;
                }
            }
        }

        todo!("{}: {token}", self.token)
    }

    pub fn type_check(
        &self,
        global_vars: &GlobalVars,
        user_defined_types: &UserDefinedTypes,
        functions: &Functions,
        interfaces: &mut Interfaces,
        interface_fn_table: &InterfaceFunctionTable,
    ) -> Result<Vec<(String, (TypedExpr, Token))>, HayError> {
        if self.requires.is_some() {
            todo!()
        }

        let mut exprs = vec![];
        for iface_impl in &self.impls {
            exprs.extend(iface_impl.type_check(
                global_vars,
                user_defined_types,
                functions,
                interfaces,
                interface_fn_table,
            )?);
        }

        Ok(exprs)
    }
}

impl InterfaceImpl {
    pub fn check_requirements<'a>(
        &self,
        token: &Token,
        user_defined_types: &UserDefinedTypes,
        free_vars: Option<&FreeVars>,
        interfaces: &'a Interfaces,
        subs: &Substitutions,
    ) -> Result<Option<&'a InterfaceImpl>, HayError> {
        match &self.requires {
            Some(requires) => {
                let mut interface_types = vec![];

                for token in requires {
                    if let Type::Interface(iface_typ) =
                        Type::from_token(&token, user_defined_types, interfaces, free_vars)?
                    {
                        interface_types.push(iface_typ);
                    } else {
                        todo!()
                    }
                }

                for iface_typ in &interface_types {
                    if let Some(iface) = interfaces.get(&iface_typ.iface) {
                        for iface_impl in &iface.impls {
                            if &iface_impl.subs == subs {
                                return Ok(Some(iface_impl));
                            }
                        }

                        return Err(HayError::new("Requirements not met...", token.loc.clone()));
                    } else {
                        todo!();
                    }
                }

                todo!()
            }
            None => Ok(None),
        }
    }

    pub fn type_check(
        &self,
        global_vars: &GlobalVars,
        user_defined_types: &UserDefinedTypes,
        functions: &Functions,
        interfaces: &mut Interfaces,
        interface_fn_table: &InterfaceFunctionTable,
    ) -> Result<Vec<(String, (TypedExpr, Token))>, HayError> {
        let mut impls = vec![];
        let mut interface_types = vec![];
        if let Some(requires) = &self.requires {
            for token in requires {
                let (iface_id, subs) = Type::from_token(
                    token,
                    user_defined_types,
                    interfaces,
                    self.free_vars.as_ref(),
                )?
                .unify_from_base(token, interfaces)?;
                interface_types.push((iface_id, subs));
            }
        }

        for (id, subs) in interface_types.clone() {
            if let Some(interface) = interfaces.get_mut(&id) {
                interface.add_generic_impl(&self.token, subs)?;
            }
        }

        for (s, f) in &self.functions {
            let id = FunctionType::name(&s, &self.subs);
            impls.push((
                id,
                f.type_check(
                    global_vars,
                    user_defined_types,
                    functions,
                    interfaces,
                    interface_fn_table,
                )?,
            ));
        }

        for (id, _) in interface_types.clone() {
            if let Some(interface) = interfaces.get_mut(&id) {
                interface.impls.pop();
            }
        }

        Ok(impls)
    }
}
