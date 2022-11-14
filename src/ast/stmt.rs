use super::arg::Arg;
use super::expr::Expr;
use super::member::Member;
use super::parser::Parser;
use crate::backend::{InitData, InitDataMap, UninitData, UninitDataMap};
use crate::error::HayError;
use crate::lex::scanner::Scanner;
use crate::lex::token::{Loc, Token};
use crate::types::{
    FnTag, GenericFunction, RecordKind, Signature, Type, TypeId, TypeMap, UncheckedFunction,
    Untyped,
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
        inputs: Vec<Arg<Untyped>>,
        outputs: Vec<Arg<Untyped>>,
        annotations: Option<Vec<Arg<Untyped>>>,
        body: Vec<Expr>,
        tags: Vec<FnTag>,
    },
    Record {
        token: Token,
        name: Token,
        annotations: Option<Vec<Arg<Untyped>>>,
        members: Vec<Member<Untyped>>,
        kind: RecordKind,
    },
    Enum {
        token: Token,
        name: Token,
        variants: Vec<Token>,
    },
    Var {
        token: Token,
        expr: Box<Expr>,
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
        annotations: Option<Vec<Arg<Untyped>>>,
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
                let members = Member::resolve(members, types, &generics)?;

                let prev = match generics.len() {
                    0 => types.insert(
                        TypeId::new(&name.lexeme),
                        Type::Record {
                            token,
                            name: name.clone(),
                            members,
                            kind,
                        },
                    ),
                    _ => types.insert(
                        TypeId::new(&name.lexeme),
                        Type::GenericRecordBase {
                            token,
                            name: name.clone(),
                            generics,
                            members,
                            kind,
                        },
                    ),
                };

                match prev {
                    None => (),
                    Some(_) => {
                        return Err(HayError::new(
                            format!("Name conflict: `{}` defined elsewhere.", name.lexeme),
                            name.loc,
                        ))
                    }
                }
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
            } => {
                let generics = Stmt::bulid_local_generics(annotations, types)?;
                let inputs = Arg::resolve(inputs, types, &generics)?;
                let outputs = Arg::resolve(outputs, types, &generics)?;

                let sig = Signature::new_maybe_generic(
                    inputs.iter().map(|arg| arg.typ.0.clone()).collect(),
                    outputs.iter().map(|arg| arg.typ.0.clone()).collect(),
                    if generics.is_empty() {
                        None
                    } else {
                        Some(generics.clone())
                    },
                );

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
            Stmt::Var { token, expr } => {
                if let Expr::Var { token, typ, ident } = *expr {
                    let inner = TypeId::from_token(&typ, types, &vec![])?;
                    let ptr = Type::Pointer {
                        inner: inner.clone(),
                    };
                    let id = ptr.id();
                    types.insert(ptr.id(), ptr);

                    let sig = Signature::new(vec![], vec![id]);

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
                        uninit_data
                            .insert(ident.lexeme.clone(), UninitData::Region(inner.size(types)?));
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
                } else {
                    return Err(HayError::new(
                        format!("{}: Logic Error -- Assertion failed. Expected Expr::Var from Stmt::Var", line!()),
                        token.loc,
                    ));
                }
            }
        }

        Ok(())
    }
}

impl std::fmt::Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Function {
                token, name, body, ..
            } => {
                writeln!(f, "[{}] fn {}:", token.loc, name.lexeme)?;
                for e in body {
                    writeln!(f, "  {e}")?;
                }

                Ok(())
            }
            Stmt::Record {
                token,
                name,
                members,
                kind,
                ..
            } => {
                writeln!(
                    f,
                    "[{}] {} {}:",
                    token.loc,
                    match kind {
                        RecordKind::Union => "union",
                        RecordKind::Struct => "struct",
                    },
                    name.lexeme
                )?;
                for mem in members {
                    writeln!(f, "  {}: {}", mem.token.lexeme, mem.ident.lexeme)?;
                }

                Ok(())
            }
            Stmt::Enum {
                token,
                name,
                variants,
            } => {
                writeln!(f, "[{}] enum {}:", token.loc, name.lexeme)?;
                for v in variants {
                    writeln!(f, "  {}", v.lexeme)?;
                }

                Ok(())
            }
            Stmt::Var { token, expr } => {
                write!(f, "[{}] {expr}", token.loc)
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
}
