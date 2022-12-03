use super::{
    EnumStmt, FunctionStmt, FunctionStubStmt, GlobalEnv, InterfaceImplStmt, InterfaceStmt,
    PreDeclarationStmt, RecordStmt, VarStmt,
};
use crate::ast::arg::UntypedArg;
use crate::ast::parser::Parser;
use crate::backend::{InitDataMap, UninitDataMap};
use crate::error::HayError;
use crate::lex::scanner::Scanner;
use crate::lex::token::Loc;
use crate::types::{Type, TypeId, TypeMap};
use std::collections::{BTreeMap, HashMap, HashSet};

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
        let prelude_path = String::from("src/libs/prelude.hay");
        let mut stmts = Stmt::from_file(&prelude_path, &mut visited)?;
        stmts.append(&mut Stmt::from_file(input_path, &mut visited)?);

        Ok(stmts)
    }

    pub fn bulid_local_generics(
        annotations: Option<Vec<UntypedArg>>,
        types: &BTreeMap<TypeId, Type>,
        impl_on: Option<&TypeId>,
    ) -> Result<Vec<TypeId>, HayError> {
        let mut out = vec![];
        match annotations {
            None => (),
            Some(annotations) => {
                for a in annotations {
                    let tid = TypeId::new(&a.token.lexeme);
                    if types.contains_key(&tid) {
                        return Err(HayError::new(format!("Generic type {} cannot be used as it has already been defined elsewhere.", a.token.lexeme), a.token.loc));
                    }
                    out.push(tid);
                }
            }
        }

        match impl_on {
            Some(tid) => match types.get(tid) {
                Some(Type::InterfaceBase(interface)) => {
                    for t in &interface.annotations {
                        out.push(t.clone());
                    }
                    for (t, _) in &interface.types {
                        out.push(t.clone());
                    }
                }
                Some(_) => unimplemented!("tid: {tid}"),
                None => unimplemented!("unrecognized type {tid}"),
            },
            None => (),
        }

        Ok(out)
    }

    pub fn build_types_and_data<'a>(
        stmts: Vec<Self>,
    ) -> Result<(TypeMap, GlobalEnv<'a>, InitDataMap, UninitDataMap), HayError> {
        let mut types = Type::new_map();
        let mut global_env = HashMap::new();
        let mut init_data = HashMap::new();
        let mut uninit_data = HashMap::new();

        for s in stmts {
            s.add_to_global_scope(
                &mut types,
                &mut global_env,
                &mut init_data,
                &mut uninit_data,
            )?;
        }

        let unimpl_decls = types
            .iter()
            .filter(|(_, t)| matches!(t, Type::RecordPreDeclaration { .. }))
            .collect::<Vec<(&TypeId, &Type)>>();

        if !unimpl_decls.is_empty() {
            let token = match unimpl_decls.first().unwrap().1 {
                Type::RecordPreDeclaration { token, .. } => token,
                _ => unreachable!(),
            };
            let mut e = HayError::new(
                "The following types were never declared:",
                token.loc.clone(),
            );
            for (tid, t) in unimpl_decls {
                match t {
                    Type::RecordPreDeclaration { token, .. } => {
                        e = e.with_hint_and_custom_note(format!("{tid}"), format!("{}", token.loc))
                    }
                    _ => unreachable!(),
                }
            }

            return Err(e);
        }

        Ok((types, global_env, init_data, uninit_data))
    }

    pub fn add_to_global_scope(
        self,
        types: &mut TypeMap,
        global_env: &mut GlobalEnv,
        init_data: &mut InitDataMap,
        uninit_data: &mut UninitDataMap,
    ) -> Result<(), HayError> {
        match self {
            Stmt::Record(stmt) => stmt.add_to_global_scope(types),
            Stmt::PreDeclaration(stmt) => stmt.add_to_global_scope(types),
            Stmt::Enum(stmt) => stmt.add_to_global_scope(types),
            Stmt::Function(stmt) => stmt.add_to_global_scope(types, global_env, None),
            Stmt::Var(stmt) => stmt.add_to_global_scope(types, global_env, init_data, uninit_data),
            Stmt::FunctionStub(stmt) => stmt.add_to_global_scope(types, global_env, None),
            Stmt::Interface(stmt) => stmt.add_to_global_scope(types, global_env),
            Stmt::InterfaceImpl(stmt) => {
                stmt.add_to_global_scope(types, global_env, init_data, uninit_data)
            }
        }
    }
}

mod tests {

    #[test]
    fn fn_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "fn_name_conflict")
    }

    #[test]
    fn enum_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "enum_name_conflict")
    }

    #[test]
    fn record_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "record_name_conflict")
    }

    #[test]
    fn var_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "var_name_conflict")
    }

    #[test]
    fn pre_declare_generics_mismatch() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "pre_declare_generics_mismatch")
    }

    #[test]
    fn pre_declare_generics_mismatch2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "pre_declare_generics_mismatch2")
    }

    #[test]
    fn pre_declare_generics_mismatch3() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "pre_declare_generics_mismatch3")
    }

    #[test]
    fn pre_declare_generics_mismatch4() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "pre_declare_generics_mismatch4")
    }

    #[test]
    fn pre_declare_kind_mismatch() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "pre_declare_kind_mismatch")
    }

    #[test]
    fn pre_declare_kind_mismatch2() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "pre_declare_kind_mismatch2")
    }

    #[test]
    fn dangling_pre_declaration() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "dangling_pre_declaration")
    }

    #[test]
    fn pre_decl_name_conflict() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("stmt", "pre_decl_name_conflict")
    }
}
