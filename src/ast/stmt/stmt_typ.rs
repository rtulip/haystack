use super::{
    EnumStmt, FunctionDescription, FunctionStmt, FunctionStubStmt, InterfaceImplStmt,
    InterfaceStmt, PreDeclarationStmt, PreDeclaredType, RecordDescription, RecordStmt, StmtKind,
    VarStmt,
};
use crate::ast::arg::UntypedArg;
use crate::ast::parser::Parser;
// use crate::backend::{InitDataMap, UninitDataMap};
use crate::error::HayError;
use crate::lex::scanner::Scanner;
use crate::lex::token::Loc;
use crate::types::{Type, TypeId, TypeVar};
use std::collections::{BTreeMap, HashMap, HashSet};

pub type Functions = BTreeMap<String, FunctionDescription>;
pub type UserDefinedTypes = BTreeMap<TypeId, TypeDescription>;

#[derive(Debug, Clone)]
pub enum TypeDescription {
    PreDeclaration(PreDeclaredType),
    Record(RecordDescription),
}

#[derive(Clone)]
pub enum Stmt {
    Interface(InterfaceStmt),
    InterfaceImpl(InterfaceImplStmt),
    FunctionStub(FunctionStubStmt),
    Function(FunctionStmt),
    PreDeclaration(PreDeclarationStmt),
    Record(RecordStmt),
    Enum(EnumStmt),
    Var(VarStmt),
}

impl Stmt {
    pub fn from_file(
        input_path: &String,
        visited: &mut HashSet<String>,
    ) -> Result<Vec<Self>, HayError> {
        if visited.contains(input_path) {
            return Ok(vec![]);
        }

        if let Ok(source) = std::fs::read_to_string(input_path) {
            visited.insert(input_path.clone());
            let scanner = Scanner::new(input_path, &source);
            let tokens = scanner.scan_tokens()?;
            let parser = Parser::new(tokens, visited);
            parser.parse()
        } else {
            Err(HayError::new(
                format!("Failed to read from file: {input_path}"),
                Loc::new(input_path, 0, 0, 0),
            ))
        }
    }

    pub fn from_file_with_prelude(input_path: &String) -> Result<Vec<Self>, HayError> {
        let mut visited = HashSet::new();
        let prelude_path = String::from("std/prelude.hay");
        let mut stmts = Stmt::from_file(&prelude_path, &mut visited)?;
        stmts.append(&mut Stmt::from_file(input_path, &mut visited)?);

        Ok(stmts)
    }

    pub fn build_types_and_data(
        stmts: Vec<Self>,
    ) -> Result<(Functions, UserDefinedTypes), HayError> {
        let mut functions = Functions::new();
        let mut user_defined_types = UserDefinedTypes::new();
        for stmt in stmts {
            match stmt {
                Stmt::Interface(iface) => (),
                Stmt::InterfaceImpl(iface_impl) => (),
                Stmt::FunctionStub(_) => todo!("FunctionStub"),
                Stmt::Function(function) => {
                    function.add_to_global_env(&user_defined_types, &mut functions)?
                }
                Stmt::PreDeclaration(predecl) => {
                    predecl.add_to_global_env(&mut user_defined_types)?
                }
                Stmt::Record(record) => record.add_to_global_env(&mut user_defined_types)?,
                Stmt::Enum(_) => todo!("Enum"),
                Stmt::Var(var) => (),
            }
        }

        assert!(user_defined_types
            .iter()
            .all(|(_, desc)| matches!(desc, TypeDescription::Record(_))));

        Ok((functions, user_defined_types))
    }
}
