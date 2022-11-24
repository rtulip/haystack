use super::arg::UntypedArg;
use super::expr::{Expr, ExprVar};
use super::member::UntypedMember;
use super::parser::Parser;
use crate::backend::{InitData, InitDataMap, UninitData, UninitDataMap};
use crate::error::HayError;
use crate::lex::scanner::Scanner;
use crate::lex::token::{Loc, Token};
use crate::types::{
    FnTag, GenericFunction, RecordKind, Signature, Type, TypeId, TypeMap, UncheckedFunction,
};
use std::collections::{BTreeMap, HashMap, HashSet};

pub type GlobalEnv<'a> = HashMap<String, (StmtKind, Signature<'a>)>;

#[derive(Debug, Clone, Copy)]
pub enum StmtKind {
    Var,
    Function,
}

#[derive(Clone)]
pub enum Stmt {
    Function {
        token: Token,
        name: Token,
        inputs: Vec<UntypedArg>,
        outputs: Vec<UntypedArg>,
        annotations: Option<Vec<UntypedArg>>,
        body: Vec<Expr>,
        tags: Vec<FnTag>,
        impl_on: Option<Token>,
    },
    PreDeclaration {
        token: Token,
        name: Token,
        kind: RecordKind,
        annotations: Option<Vec<UntypedArg>>,
    },
    Record {
        token: Token,
        name: Token,
        annotations: Option<Vec<UntypedArg>>,
        members: Vec<UntypedMember>,
        kind: RecordKind,
    },
    Enum {
        token: Token,
        name: Token,
        variants: Vec<Token>,
    },
    Var {
        token: Token,
        expr: ExprVar,
    },
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
            let stmts = parser.parse()?;

            Ok(stmts)
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

    fn bulid_local_generics(
        annotations: Option<Vec<UntypedArg>>,
        types: &BTreeMap<TypeId, Type>,
    ) -> Result<Vec<TypeId>, HayError> {
        match annotations {
            None => Ok(vec![]),
            Some(annotations) => {
                let mut out = vec![];
                for a in annotations {
                    if types.contains_key(&TypeId::new(&a.token.lexeme)) {
                        return Err(HayError::new(format!("Generic type {} cannot be used as it has already been defined elsewhere.", a.token.lexeme), a.token.loc));
                    }
                    out.push(TypeId(a.token.lexeme));
                }
                Ok(out)
            }
        }
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

        if unimpl_decls.len() != 0 {
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
        types: &mut BTreeMap<TypeId, Type>,
        global_env: &mut HashMap<String, (StmtKind, Signature)>,
        init_data: &mut HashMap<String, InitData>,
        uninit_data: &mut HashMap<String, UninitData>,
    ) -> Result<(), HayError> {
        match self {
            Stmt::Record {
                token,
                name,
                annotations,
                members,
                kind,
            } => {
                let generics = Stmt::bulid_local_generics(annotations, types)?;
                let members = UntypedMember::resolve(members, types, &generics)?;

                let prev = match generics.len() {
                    0 => types.insert(
                        TypeId::new(&name.lexeme),
                        Type::Record {
                            token: token.clone(),
                            name: name.clone(),
                            members,
                            kind,
                        },
                    ),
                    _ => types.insert(
                        TypeId::new(&name.lexeme),
                        Type::GenericRecordBase {
                            token: token.clone(),
                            name: name.clone(),
                            generics: generics.clone(),
                            members,
                            kind,
                        },
                    ),
                };

                match prev {
                    None => (),
                    Some(Type::RecordPreDeclaration {
                        token: pre_decl_token,
                        kind: pre_decl_kind,
                        generics: pre_decl_generics,
                        ..
                    }) => {
                        TypeId::new(&name.lexeme).validate_redeclaration(
                            &token,
                            &pre_decl_kind,
                            &pre_decl_token,
                            &pre_decl_generics,
                            &kind,
                            &token,
                            if generics.len() == 0 {
                                None
                            } else {
                                Some(&generics)
                            },
                        )?;
                    }
                    Some(_) => {
                        return Err(HayError::new(
                            format!("Name conflict: `{}` defined elsewhere.", name.lexeme),
                            name.loc,
                        ))
                    }
                }
            }
            Stmt::PreDeclaration {
                token,
                name,
                kind,
                annotations,
            } => {
                let tid = TypeId::new(&name.lexeme);
                let generics = Stmt::bulid_local_generics(annotations, types)?;
                match types.get(&tid) {
                    Some(Type::Record {
                        token: decl_token,
                        kind: decl_kind,
                        ..
                    }) => {
                        tid.validate_redeclaration(
                            &token, &kind, &token, &generics, decl_kind, decl_token, None,
                        )?;
                        return Ok(());
                    }
                    Some(Type::GenericRecordBase {
                        token: decl_token,
                        kind: decl_kind,
                        generics: decl_generics,
                        ..
                    }) => {
                        tid.validate_redeclaration(
                            &token,
                            &kind,
                            &token,
                            &generics,
                            decl_kind,
                            decl_token,
                            Some(decl_generics),
                        )?;
                        return Ok(());
                    }
                    Some(_) => {
                        return Err(HayError::new(
                            format!("Name conflict: `{}` defined elsewhere.", name.lexeme),
                            name.loc,
                        ))
                    }
                    None => (),
                }

                types.insert(
                    tid,
                    Type::RecordPreDeclaration {
                        token,
                        name,
                        kind,
                        generics,
                    },
                );
            }

            Stmt::Enum {
                token,
                name,
                variants,
            } => {
                let tid = TypeId::new(&name.lexeme);
                let t = Type::Enum {
                    token,
                    name: name.clone(),
                    variants,
                };
                match types.insert(tid, t) {
                    None => (),
                    Some(_) => {
                        return Err(HayError::new(
                            format!("Name conflict. `{}` defined elsewhere", name.lexeme),
                            name.loc,
                        ))
                    }
                }
            }
            Stmt::Function {
                token,
                name,
                inputs,
                outputs,
                annotations,
                body,
                tags,
                impl_on,
            } => {
                let generics = Stmt::bulid_local_generics(annotations, types)?;
                let inputs = UntypedArg::resolve(inputs, types, &generics)?;
                let outputs = UntypedArg::resolve(outputs, types, &generics)?;

                let sig = Signature::new_maybe_generic(
                    inputs.iter().map(|arg| arg.typ.clone()).collect(),
                    outputs.iter().map(|arg| arg.typ.clone()).collect(),
                    if generics.is_empty() {
                        None
                    } else {
                        Some(generics.clone())
                    },
                );

                let impl_on = match impl_on {
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
                            token,
                            name: name.clone(),
                            inputs,
                            outputs,
                            body,
                            generic_map: None,
                            tags,
                            impl_on,
                        },
                    }
                } else {
                    Type::GenericFunction {
                        func: GenericFunction {
                            token,
                            name: name.clone(),
                            inputs,
                            outputs,
                            generics,
                            body,
                            tags,
                            impl_on,
                        },
                    }
                };

                match types.insert(TypeId::new(&name.lexeme), typ) {
                    None => {
                        global_env.insert(name.lexeme, (StmtKind::Function, sig));
                    }
                    Some(_) => {
                        return Err(HayError::new(
                            format!(
                                "Function name conflict. `{}` defined elsewhere",
                                name.lexeme
                            ),
                            name.loc,
                        ));
                    }
                }
            }
            Stmt::Var {
                expr: ExprVar { token, typ, ident },
                ..
            } => {
                let inner = TypeId::from_token(&typ, types, &vec![])?;
                let inner_size = inner.size(types)?;
                let sig = Signature::new(vec![], vec![inner.ptr_of(true, types)]);

                if let Some((dimension, tt)) = typ.dimension()? {
                    let inner_typ = TypeId::from_type_token(&typ, &tt, types, &vec![])?;
                    let inner_size = inner_typ.size(types)?;

                    let data_id = uninit_data.len();
                    uninit_data.insert(
                        format!("data_{data_id}"),
                        UninitData::Region(inner_size * dimension),
                    );
                    init_data.insert(
                        ident.lexeme.clone(),
                        InitData::Arr {
                            size: dimension,
                            pointer: format!("data_{data_id}"),
                        },
                    );
                } else {
                    uninit_data.insert(ident.lexeme.clone(), UninitData::Region(inner_size));
                }

                match global_env.insert(ident.lexeme.clone(), (StmtKind::Var, sig)) {
                    None => (),
                    Some(_) => {
                        return Err(HayError::new(
                            format!("Var conflict. `{}` defined elsewhere", ident.lexeme),
                            token.loc,
                        ))
                    }
                }
            }
        }

        Ok(())
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
}
