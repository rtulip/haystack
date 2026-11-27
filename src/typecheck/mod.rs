use std::{
    assert_eq,
    collections::{HashMap, HashSet},
    dbg, eprintln,
    fmt::Pointer,
    format, todo, unreachable, vec,
};

use crate::{
    error::HayError,
    lex::token::{Intrinsic, Literal, Token},
    parser::{
        argument::Argument,
        expr::{ParseExpr, ParseExprKind},
        signature::Signature,
        statement::{
            EnumDefinition, FunctionDeclaration, FunctionDefinition, InterfaceDefinition,
            InterfaceFunctionDefinition, InterfaceImpl as ParseInterfaceImpl, Statememt,
            StructDefinition,
        },
        typ::{ParamatarizedIdent, ParseType, ParseTypePath},
    },
    typecheck::{
        constraint::ConstraintOrigin,
        environment::{
            Coherence, Environment, Interface, InterfaceImpl, Scheme, TypeClassId, TypePredicate,
        },
        expr::{
            Expr, ExprKind, IfExpr, MatchCase, MatchExpr, MatchPattern, Place, PlaceProjection, Var,
        },
        func::Function,
        typ::{
            typ::{
                BuiltinType, EnumDef, EnumType, FunctionType, Mutability, Projection, StackState,
                StructDef, Type,
            },
            type_id::{EnumId, StructId, TypeId},
            type_var::TypeVar,
        },
        typestore::{
            inference::{InferenceError, InferenceErrorKind},
            MemberLookup, TypeStore,
        },
    },
};

pub mod constraint;
pub mod environment;
pub mod expr;
pub mod func;
pub mod typ;
pub mod typestore;
pub mod union_find;

pub type TypedFunction<'src> = Function<'src, (Token<'src>, TypeId)>;

pub struct TypeCheck<'src> {
    typestore: TypeStore<Token<'src>>,
    env: Environment,
    symbols: SymbolTable<'src>,
    scope: Scope<'src>,
    bindings: HashMap<Token<'src>, Vec<TypeVar>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TypeInfo<'src> {
    name: String,
    token: Option<Token<'src>>,
}

struct VarInfo<'src> {
    name: &'src str,
    token: Token<'src>,
}

struct SymbolTable<'src> {
    types: HashMap<TypeId, TypeInfo<'src>>,
    vars: HashMap<Var, VarInfo<'src>>,
    enums: HashMap<EnumId, Token<'src>>,
    structs: HashMap<StructId, Token<'src>>,
    interfaces: HashMap<TypeClassId, Token<'src>>,
    variants: HashMap<(EnumId, usize), Token<'src>>,
    members: HashMap<(StructId, usize), Token<'src>>,
}

impl<'src> SymbolTable<'src> {
    pub fn new(typestore: &mut TypeStore<Token<'src>>) -> Self {
        let types = HashMap::from([
            (
                typestore.intern(Type::u8()),
                TypeInfo {
                    name: "u8".to_owned(),
                    token: None,
                },
            ),
            (
                typestore.intern(Type::u64()),
                TypeInfo {
                    name: "u64".to_owned(),
                    token: None,
                },
            ),
            (
                typestore.intern(Type::bool()),
                TypeInfo {
                    name: "bool".to_owned(),
                    token: None,
                },
            ),
            (
                typestore.intern(Type::char()),
                TypeInfo {
                    name: "char".to_owned(),
                    token: None,
                },
            ),
        ]);
        SymbolTable {
            types,
            vars: HashMap::new(),
            enums: HashMap::new(),
            structs: HashMap::new(),
            variants: HashMap::new(),
            members: HashMap::new(),
            interfaces: HashMap::new(),
        }
    }

    pub fn insert_tid(&mut self, tid: TypeId, info: TypeInfo<'src>) -> Option<TypeInfo<'src>> {
        self.types.insert(tid, info)
    }

    pub fn insert_cid(&mut self, cid: TypeClassId, token: Token<'src>) -> Option<Token<'src>> {
        self.interfaces.insert(cid, token)
    }

    pub fn insert_variant(
        &mut self,
        eid: EnumId,
        variant: usize,
        token: Token<'src>,
    ) -> Option<Token<'src>> {
        self.variants.insert((eid, variant), token)
    }

    pub fn insert_member(
        &mut self,
        sid: StructId,
        member: usize,
        token: Token<'src>,
    ) -> Option<Token<'src>> {
        self.members.insert((sid, member), token)
    }

    pub fn insert_struct(&mut self, sid: StructId, token: Token<'src>) -> Option<Token<'src>> {
        self.structs.insert(sid, token)
    }

    pub fn insert_var(&mut self, var: Var, info: VarInfo<'src>) -> Option<VarInfo<'src>> {
        self.vars.insert(var, info)
    }
}

#[derive(Debug, Clone)]
struct InterfaceFnInfo<'src> {
    token: Token<'src>,
    tid: TypeId,
    params: Vec<ParseType<'src>>,
    predicates: Vec<TypePredicate>,
    body: Option<Expr<'src, Token<'src>>>,
}

type InterfaceFnsLookup<'src> = HashMap<&'src str, InterfaceFnInfo<'src>>;

#[derive(Debug, Clone)]
struct InterfaceInfo<'src> {
    cid: TypeClassId,
    fns: InterfaceFnsLookup<'src>,
    associated_types: Vec<&'src str>,
}

struct ScopeLayer<'src> {
    types: HashMap<String, (TypeId, Option<Token<'src>>)>,
    vars: HashMap<&'src str, (Var, Token<'src>)>,
    interfaces: HashMap<&'src str, InterfaceInfo<'src>>,
    variants: HashMap<(EnumId, &'src str), usize>,
    members: HashMap<(StructId, &'src str), usize>,
}

struct Scope<'src> {
    scopes: Vec<ScopeLayer<'src>>,
}

impl<'src> MemberLookup for Scope<'src> {
    fn get_member_idx(&self, sid: StructId, member: &str) -> Option<usize> {
        self.get_member(sid, member)
    }
}

enum Scoping {
    Global,
    Local,
}

impl<'src> Scope<'src> {
    pub fn new(typestore: &mut TypeStore<Token<'src>>) -> Self {
        let types = HashMap::from([
            ("u8".to_owned(), (typestore.intern(Type::u8()), None)),
            ("u64".to_owned(), (typestore.intern(Type::u64()), None)),
            ("bool".to_owned(), (typestore.intern(Type::bool()), None)),
            ("char".to_owned(), (typestore.intern(Type::char()), None)),
            (
                "...".to_owned(),
                (typestore.intern(Type::Builtin(BuiltinType::CVarArgs)), None),
            ),
        ]);

        Scope {
            scopes: vec![ScopeLayer {
                types,
                vars: HashMap::new(),
                variants: HashMap::new(),
                members: HashMap::new(),
                interfaces: HashMap::new(),
            }],
        }
    }

    fn get_tid(&self, name: &String) -> Option<(TypeId, Option<Token<'src>>)> {
        self.scopes
            .iter()
            .rev()
            .find(|scope| scope.types.contains_key(name))
            .map(|scope| scope.types.get(name).unwrap().clone())
    }

    fn get_var(&self, name: &str) -> Option<(Var, Token<'src>)> {
        self.scopes
            .iter()
            .rev()
            .find(|scope| scope.vars.contains_key(name))
            .map(|scope| scope.vars.get(name).unwrap().clone())
    }

    fn get_cid(&self, name: &str) -> Option<&InterfaceInfo<'src>> {
        let x = self
            .scopes
            .iter()
            .rev()
            .find(|scope| scope.interfaces.contains_key(name))
            .map(|scope| scope.interfaces.get(name).unwrap());
        x
    }

    fn insert_cid(
        &mut self,
        name: &'src str,
        info: InterfaceInfo<'src>,
    ) -> Option<InterfaceInfo<'src>> {
        self.scopes
            .last_mut()
            .unwrap()
            .interfaces
            .insert(name, info)
    }

    fn insert_tid(
        &mut self,
        name: String,
        tid: TypeId,
        token: Token<'src>,
    ) -> Option<(TypeId, Option<Token<'src>>)> {
        self.scopes
            .last_mut()
            .unwrap()
            .types
            .insert(name, (tid, Some(token)))
    }

    fn insert_variant(&mut self, eid: EnumId, name: &'src str, variant: usize) -> Option<usize> {
        self.scopes
            .last_mut()
            .unwrap()
            .variants
            .insert((eid, name), variant)
    }

    fn insert_member(&mut self, sid: StructId, name: &'src str, member: usize) -> Option<usize> {
        self.scopes
            .last_mut()
            .unwrap()
            .members
            .insert((sid, name), member)
    }

    fn get_variant(&mut self, eid: EnumId, name: &'src str) -> Option<usize> {
        self.scopes
            .iter()
            .rev()
            .find(|scope| scope.variants.contains_key(&(eid, name)))
            .map(|scope| *scope.variants.get(&(eid, name)).unwrap())
    }

    fn get_member(&self, sid: StructId, name: &'src str) -> Option<usize> {
        self.scopes
            .iter()
            .rev()
            .find(|scope| scope.members.contains_key(&(sid, name)))
            .map(|scope| *scope.members.get(&(sid, name)).unwrap())
    }

    fn insert_var(
        &mut self,
        name: &'src str,
        var: Var,
        token: Token<'src>,
    ) -> Option<(Var, Token<'src>)> {
        self.scopes
            .last_mut()
            .unwrap()
            .vars
            .insert(name, (var, token))
    }

    fn insert_global_var(
        &mut self,
        name: &'src str,
        var: Var,
        token: Token<'src>,
    ) -> Option<(Var, Token<'src>)> {
        self.scopes
            .first_mut()
            .unwrap()
            .vars
            .insert(name, (var, token))
    }

    fn new_scope(&mut self) {
        self.scopes.push(ScopeLayer {
            types: HashMap::new(),
            vars: HashMap::new(),
            variants: HashMap::new(),
            members: HashMap::new(),
            interfaces: HashMap::new(),
        })
    }

    fn end_scope(&mut self) {
        assert!(self.scopes.len() > 1);
        self.scopes.pop();
    }
}

impl<'src> TypeCheck<'src> {
    fn insert_var(&mut self, token: &Token<'src>, scoping: Scoping) -> Result<Var, HayError> {
        let insert_fn = match scoping {
            Scoping::Global => Scope::insert_global_var,
            Scoping::Local => Scope::insert_var,
        };

        let var = self.env.fresh_var();
        if let Some((var, tok)) = insert_fn(&mut self.scope, token.lexeme, var, token.clone()) {
            return Err(HayError::new(
                format!("`{}` already defined", token.lexeme),
                token.loc.clone(),
            )
            .with_hint(format!(
                "`{}` was previously defined here: {}",
                token.lexeme, tok.loc
            )));
        }

        assert!(self
            .symbols
            .insert_var(
                var,
                VarInfo {
                    name: token.lexeme,
                    token: token.clone()
                }
            )
            .is_none());
        Ok(var)
    }

    fn insert_tyvar_to_scope(
        &mut self,
        tyvar: TypeVar,
        token: Token<'src>,
    ) -> Result<TypeVar, HayError> {
        let tid = self.typestore.intern(Type::TypeVar(tyvar));

        if let Some((tid, tok)) = self
            .scope
            .insert_tid(token.lexeme.into(), tid, token.clone())
        {
            let mut e = HayError::new(
                format!("Type `{}` already defined", token.lexeme),
                token.loc.clone(),
            );
            if let Some(tok) = tok {
                e = e.with_hint(format!(
                    "Type `{}` was previously defined here: {}",
                    token.lexeme, tok.loc
                ));
            }

            return Err(e);
        }
        let type_info = TypeInfo {
            name: token.lexeme.into(),
            token: Some(token),
        };

        if let Some(info) = self.symbols.insert_tid(tid, type_info.clone()) {
            assert!(info == type_info)
        }
        Ok(tyvar)
    }

    fn insert_param_ty(&mut self, ty: &ParseType<'src>) -> Result<TypeVar, HayError> {
        let tyvar = self.typestore.fresh_type_var();
        self.insert_tyvar_to_scope(tyvar, ty.token())
    }

    fn get_metatype_tid(&mut self, ident: &ParamatarizedIdent<'src>) -> Result<TypeId, HayError> {
        assert_eq!(ident.base.lexeme, "Type");

        if ident.params.len() != 1 {
            return Err(HayError::new(
                "Wrong number of type parameters for `Type`",
                ident.base.loc.clone(),
            )
            .with_hint("Expected a single parameter: `Type<T>`"));
        }

        let inner_tid = self.get_tid(&ident.params[0])?;
        let tid = self
            .typestore
            .metatype(&self.env, inner_tid, None, &ident.base)
            .map_err(|_| todo!())?;

        return Ok(tid);
    }

    fn get_paramatarized_ident_tid(
        &mut self,
        ident: &ParamatarizedIdent<'src>,
    ) -> Result<TypeId, HayError> {
        if ident.base.lexeme == "Type" {
            return self.get_metatype_tid(ident);
        }

        let (base_tid, _) = self
            .scope
            .get_tid(&ident.base.lexeme.into())
            .ok_or(HayError::new(
                format!("Unknown type `{}`", ident.base.lexeme),
                ident.base.loc.clone(),
            ))?;

        let mut params = vec![];
        for param in &ident.params {
            params.push(self.get_tid(param)?);
        }

        if params.len() == 0 {
            Ok(base_tid)
        } else {
            self.typestore
                .type_instance(&self.env, base_tid, Some(params), &ident.base)
                .map_err(|kind| {
                    self.map_inference_err(InferenceError {
                        kind,
                        origin: ConstraintOrigin::TypeInstance(base_tid),
                        meta: ident.base.clone(),
                    })
                })
        }
    }

    fn get_enum_variant(
        &mut self,
        enum_tid: TypeId,
        eid: EnumId,
        ident: &ParamatarizedIdent<'src>,
    ) -> Result<TypeId, HayError> {
        let variant_id = match self.scope.get_variant(eid, ident.base.lexeme) {
            Some(variant_id) => variant_id,
            None => {
                return Err(HayError::new(
                    format!(
                        "Unknown variant `{}` for enum `{}`",
                        ident.base.lexeme, self.symbols.enums[&eid].lexeme,
                    ),
                    ident.base.loc.clone(),
                ))
            }
        };

        let tid = self
            .typestore
            .get_enum_variant(enum_tid, variant_id, &ident.base)
            .map_err(|e| self.map_inference_err(e))?;
        Ok(tid)
    }

    fn get_next_tid(
        &mut self,
        tid: TypeId,
        ident: &ParamatarizedIdent<'src>,
    ) -> Result<TypeId, HayError> {
        match self
            .typestore
            .get(tid)
            .expect("TypeIds should be resolvable")
        {
            Type::Enum(e) => self.get_enum_variant(tid, e.eid, ident),
            _ => Err(HayError::new(
                format!(
                    "Type `{}` doesn't have an inner type `{}`",
                    self.format_tid(tid),
                    ident.base.lexeme
                ),
                ident.base.loc.clone(),
            )),
        }
    }

    fn get_type_path_tid(&mut self, path: &ParseTypePath<'src>) -> Result<TypeId, HayError> {
        assert!(path.path.len() >= 2);

        let mut tid = self.get_paramatarized_ident_tid(&path.path[0])?;

        for ident in &path.path[1..] {
            tid = self.get_next_tid(tid, ident)?;
        }

        Ok(tid)
    }

    fn get_tid(&mut self, ty: &ParseType<'src>) -> Result<TypeId, HayError> {
        match ty {
            ParseType::Simple(ident) => Ok(self.get_paramatarized_ident_tid(ident)?),
            ParseType::Path(path) => self.get_type_path_tid(path),
            ParseType::Pointer { inner, mutable } => {
                let inner = self.get_tid(inner)?;
                let tid = self.typestore.pointer(
                    inner,
                    if *mutable {
                        Mutability::Mutable
                    } else {
                        Mutability::Const
                    },
                );

                Ok(tid)
            }
            ParseType::Tuple(tys) => {
                let mut tids = vec![];
                for ty in tys {
                    tids.push(self.get_tid(ty)?);
                }

                let tid = self.typestore.tuple(tids);
                Ok(tid)
            }
            ParseType::Function(_, sig) => {
                let mut input = vec![];

                for arg in &sig.input {
                    input.push(self.get_tid(&arg.typ)?);
                }

                let output = match &sig.output {
                    Some(tys) => {
                        let mut output = vec![];
                        for ty in tys {
                            output.push(self.get_tid(&ty)?);
                        }

                        StackState::Reachable(output)
                    }
                    None => StackState::Unreachable,
                };

                Ok(self.typestore.function(FunctionType { input, output }))
            }
        }
    }

    fn get_function_tid(&mut self, signature: &Signature<'src>) -> Result<TypeId, HayError> {
        let function_type = FunctionType {
            input: signature
                .input
                .iter()
                .map(|arg| self.get_tid(&arg.typ))
                .collect::<Result<Vec<_>, _>>()?,

            output: match &signature.output {
                Some(output) => output
                    .iter()
                    .map(|ty| self.get_tid(ty))
                    .collect::<Result<Vec<_>, _>>()?
                    .into(),
                None => StackState::Unreachable,
            },
        };

        Ok(self.typestore.function(function_type))
    }

    fn lower_expr(&mut self, expr: &ParseExpr<'src>) -> Result<Expr<'src, Token<'src>>, HayError> {
        match &expr.kind {
            ParseExprKind::Place(place) => {
                let Some((var, _)) = self.scope.get_var(place.base) else {
                    todo!("{}: No var found for `{}`", place.base, expr.token)
                };

                Ok(Expr::place(
                    var,
                    place.projections.clone(),
                    expr.token.clone(),
                ))
            }
            ParseExprKind::Projection(proj) => Ok(Expr::projection(proj, expr.token.clone())),
            ParseExprKind::Literal(literal) => match literal {
                Literal::Bool(b) => Ok(Expr::bool(*b, expr.token.clone())),
                Literal::Char(c) => Ok(Expr::char(*c, expr.token.clone())),
                Literal::String(s) => Ok(Expr::string(s, expr.token.clone())),
                Literal::CString(s) => Ok(Expr::cstring(s, expr.token.clone())),
                Literal::U8(n) => Ok(Expr::u8(*n, expr.token.clone())),
                Literal::U64(n) => Ok(Expr::u64(*n, expr.token.clone())),
            },
            ParseExprKind::Block(parse_exprs) => {
                self.scope.new_scope();
                let mut exprs = vec![];
                for expr in parse_exprs {
                    exprs.push(self.lower_expr(expr)?);
                }
                self.scope.end_scope();
                Ok(Expr::block(exprs, expr.token.clone()))
            }
            ParseExprKind::Type(ty) => {
                let tid = self.get_tid(ty)?;
                let tid = self
                    .typestore
                    .metatype(&self.env, tid, None, &expr.token)
                    .map_err(|e| todo!("{e:?}"))?;
                Ok(Expr::typ(tid, expr.token.clone()))
            }
            ParseExprKind::Pack => Ok(Expr::pack(expr.token.clone())),
            ParseExprKind::Unpack => Ok(Expr::unpack(expr.token.clone())),
            ParseExprKind::Call => Ok(Expr::call(expr.token.clone())),
            ParseExprKind::SizeOf => Ok(Expr::sizeof(expr.token.clone())),
            ParseExprKind::Read => Ok(Expr::read(expr.token.clone())),
            ParseExprKind::Write => Ok(Expr::write(expr.token.clone())),
            ParseExprKind::Intrinsic(Intrinsic::UncheckedCast) => {
                Ok(Expr::unchecked_cast(expr.token.clone()))
            }
            ParseExprKind::Intrinsic(Intrinsic::U64Add) => {
                let u64_tid = self.typestore.intern(Type::u64());
                let tid = self.typestore.function(FunctionType {
                    input: vec![u64_tid, u64_tid],
                    output: vec![u64_tid].into(),
                });
                Ok(Expr::add(tid, expr.token.clone()))
            }
            ParseExprKind::Intrinsic(Intrinsic::PtrAdd) => {
                let t_tyvar = self.typestore.fresh_type_var();
                let t_tid = self.typestore.intern(Type::TypeVar(t_tyvar));
                let t_ptr_tid = self.typestore.pointer(t_tid, Mutability::Const);
                let u64_tid = self.typestore.intern(Type::u64());
                let tid = self.typestore.function(FunctionType {
                    input: vec![t_ptr_tid, u64_tid],
                    output: vec![t_ptr_tid].into(),
                });
                Ok(Expr::add(tid, expr.token.clone()))
            }
            ParseExprKind::Intrinsic(Intrinsic::PtrMutAdd) => {
                let t_tyvar = self.typestore.fresh_type_var();
                let t_tid = self.typestore.intern(Type::TypeVar(t_tyvar));
                let t_ptr_tid = self.typestore.pointer(t_tid, Mutability::Mutable);
                let u64_tid = self.typestore.intern(Type::u64());
                let tid = self.typestore.function(FunctionType {
                    input: vec![t_ptr_tid, u64_tid],
                    output: vec![t_ptr_tid].into(),
                });
                Ok(Expr::add(tid, expr.token.clone()))
            }
            ParseExprKind::Intrinsic(Intrinsic::U64Sub) => {
                let tid = self.typestore.intern(Type::u64());
                Ok(Expr::sub(tid, expr.token.clone()))
            }
            ParseExprKind::Intrinsic(Intrinsic::U64Mul) => {
                let tid = self.typestore.intern(Type::u64());
                Ok(Expr::mul(tid, expr.token.clone()))
            }
            ParseExprKind::Intrinsic(Intrinsic::U64Eq) => {
                let tid = self.typestore.intern(Type::u64());
                Ok(Expr::eq(tid, expr.token.clone()))
            }
            ParseExprKind::Intrinsic(Intrinsic::U64Lt) => {
                let tid = self.typestore.intern(Type::u64());
                Ok(Expr::lt(tid, expr.token.clone()))
            }
            ParseExprKind::Tuple(parse_exprs) => {
                let mut exprs = vec![];
                for expr in parse_exprs {
                    exprs.push(self.lower_expr(expr)?);
                }

                Ok(Expr::tuple(exprs, expr.token.clone()))
            }
            ParseExprKind::As(idents) => {
                let mut vars = vec![];
                for ident in idents {
                    let var = self.env.fresh_var();
                    self.scope.insert_var(ident.lexeme, var, ident.clone());
                    self.symbols.insert_var(
                        var,
                        VarInfo {
                            name: ident.lexeme,
                            token: ident.clone(),
                        },
                    );
                    vars.push(var);
                }

                Ok(Expr::bind(vars, expr.token.clone()))
            }
            ParseExprKind::TakeRef { mutable, place } => {
                let Some((var, _)) = self.scope.get_var(place.base) else {
                    todo!("Unknown var `{}`", place.base);
                };

                let place = Place {
                    base: var,
                    projections: place
                        .projections
                        .iter()
                        .map(|name| PlaceProjection::Named(*name))
                        .collect(),
                };

                let mutability = if *mutable {
                    Mutability::Mutable
                } else {
                    Mutability::Const
                };

                Ok(Expr::take_ref(place, mutability, expr.token.clone()))
            }
            ParseExprKind::If(if_expr) => {
                self.scope.new_scope();
                let mut then = vec![];
                for expr in &if_expr.then {
                    then.push(self.lower_expr(expr)?);
                }
                self.scope.end_scope();

                let then = Expr::block(then, expr.token.clone());

                self.scope.new_scope();
                let mut otherwise = vec![];
                for expr in &if_expr.finally {
                    otherwise.push(self.lower_expr(expr)?);
                }
                self.scope.end_scope();

                let mut otherwise = Expr::block(otherwise, expr.token.clone());

                for branch in if_expr.otherwise.iter().rev() {
                    self.scope.new_scope();
                    let mut then = vec![];
                    for expr in &branch.body {
                        then.push(self.lower_expr(expr)?);
                    }
                    self.scope.end_scope();

                    let if_expr = Expr::iff(
                        Expr::block(then, branch.token.clone()),
                        otherwise,
                        branch.token.clone(),
                    );

                    self.scope.new_scope();
                    let mut block = vec![];
                    for expr in &branch.condition {
                        block.push(self.lower_expr(expr)?);
                    }
                    self.scope.end_scope();
                    block.push(if_expr);

                    otherwise = Expr::block(block, branch.token.clone());
                }

                Ok(Expr::iff(then, otherwise, expr.token.clone()))
            }
            ParseExprKind::Match(m) => {
                let mut cases = vec![];
                for (ty, expr) in &m.cases {
                    let tid = self.get_tid(ty)?;
                    let expr = self.lower_expr(expr)?;

                    cases.push(MatchCase::new(MatchPattern::Variant(tid), expr));
                }

                let finally = match &m.finally {
                    Some(expr) => Some(self.lower_expr(&expr)?),
                    None => None,
                };

                Ok(Expr::match_(
                    MatchExpr::new(cases, finally),
                    expr.token.clone(),
                ))
            }
            ParseExprKind::While { condition, body } => {
                let mut cond = vec![];
                for c in condition {
                    cond.push(self.lower_expr(c)?)
                }

                let body = self.lower_expr(body)?;

                Ok(Expr::while_(cond, body, expr.token.clone()))
            }
        }
    }

    fn format_enum(&mut self, eid: EnumId, args: Vec<TypeId>, variant: Option<usize>) -> String {
        let base_enum_def = self.typestore.get_enum_def(eid).clone();
        let base_enum_ty = EnumType {
            eid,
            args: base_enum_def
                .vars
                .iter()
                .map(|tid| self.typestore.intern(Type::TypeVar(*tid)))
                .collect(),
        };
        let tid = self.typestore.intern(Type::Enum(base_enum_ty));

        let mut s = self.format_tid(tid);

        if args.len() > 0 {
            s = format!("{s}{}", self.format_stack(&args, '<', '>'))
        }

        if let Some(variant) = variant {
            let token = self
                .symbols
                .variants
                .get(&(eid, variant))
                .expect("Variant should have already been checked");
            s = format!("{s}::{}", token.lexeme);
        }

        s
    }

    fn format_tid_from_type(&mut self, tid: TypeId) -> String {
        match self
            .typestore
            .get(tid)
            .expect("TypeIds should be resolvable")
            .clone()
        {
            Type::TypeVar(tyvar) | Type::RigidTypeVar(tyvar) => {
                let tid = self.typestore.intern(Type::TypeVar(tyvar));
                if let Some(info) = self.symbols.types.get(&tid) {
                    info.name.clone()
                } else {
                    self.typestore.fmt(tid)
                }
            }
            Type::Builtin(b) => match b {
                BuiltinType::U8 => todo!(),
                BuiltinType::U64 => todo!(),
                BuiltinType::Bool => todo!(),
                BuiltinType::Char => "char".into(),
                BuiltinType::CVarArgs => "...".into(),
            },
            Type::Function(func) => {
                let mut s = format!("fn{}", self.format_stack(&func.input, '(', ')'));
                match &func.output {
                    StackState::Reachable(tids) if tids.is_empty() => (),
                    _ => s = format!("{s} -> {}", self.format_stack_state(&func.output, '[', ']')),
                }
                s
            }
            Type::Pointer(p) => {
                if p.mutability == Mutability::Const {
                    format!("&{}", self.format_tid(p.tid))
                } else {
                    format!("&mut {}", self.format_tid(p.tid))
                }
            }
            Type::Struct(s) => {
                let args = if s.args.is_empty() {
                    String::new()
                } else {
                    self.format_stack(&s.args, '<', '>')
                };

                let Some(token) = self.symbols.structs.get(&s.sid) else {
                    todo!();
                };

                format!("{}{args}", token.lexeme)
            }
            Type::Enum(e) => self.format_enum(e.eid, e.args, None),
            Type::Tuple(tids) => self.format_stack(&tids, '(', ')'),
            Type::Variant(v) => self.format_enum(v.eid, v.args, Some(v.variant)),
            Type::Type(tid) => {
                format!("Type<{}>", self.format_tid(tid))
            }
            Type::Projection(p) => {
                let args = self.format_stack(&p.args, '<', '>');
                let iface = self.symbols.interfaces.get(&p.cid).unwrap();
                format!("{}{args}::{}", iface.lexeme, p.idx)
            }
        }
    }

    fn format_tid(&mut self, tid: TypeId) -> String {
        let tid = self
            .typestore
            .normalize_ty(&self.env, tid, &Token::default())
            .unwrap()
            .1;

        if let Some(info) = self.symbols.types.get(&tid) {
            info.name.clone()
        } else {
            if let Some((origin, _)) = self.typestore.tyvar_origin.get(&tid) {
                return self.format_tid(*origin);
            }

            self.format_tid_from_type(tid)
        }
    }

    fn format_stack(&mut self, stack: &Vec<TypeId>, open: char, close: char) -> String {
        if stack.is_empty() {
            format!("{open}{close}")
        } else if stack.len() == 1 {
            format!("{open}{}{close}", self.format_tid(stack[0]))
        } else {
            let mut s = format!("{open}{}", self.format_tid(stack[0]));
            for tid in &stack[1..] {
                s = format!("{s} {}", self.format_tid(*tid));
            }
            s = format!("{s}{close}");
            s
        }
    }

    fn format_stack_state(&mut self, stack: &StackState, open: char, close: char) -> String {
        match stack {
            StackState::Reachable(stack) => self.format_stack(stack, open, close),
            StackState::Unreachable => String::from("!"),
        }
    }

    fn format_predicate(&mut self, cid: TypeClassId, args: Vec<TypeId>) -> String {
        let Some(Token { lexeme, .. }) = self.symbols.interfaces.get(&cid).cloned() else {
            todo!("Can't fid {cid:?}");
        };

        let predicate = self.format_stack(&args, '<', '>');
        format!("{lexeme}{predicate}")
        // todo!()
    }

    fn map_inference_err(&mut self, e: InferenceError<Token<'src>>) -> HayError {
        match (e.origin, e.kind) {
            (
                ConstraintOrigin::FunctionCall { input, expected },
                InferenceErrorKind::TypesIncompatible {
                    expected: expected_tid,
                    found: found_tid,
                },
            ) => {
                let expected = self.format_stack(&expected, '[', ']');
                let input = self.format_stack(&input, '[', ']');

                let expected_ty = self.format_tid(expected_tid);
                let incompatible_ty = self.format_tid(found_tid);

                HayError::new(
                    format!("Incorrect Arguments when calling `{}`", e.meta.lexeme),
                    e.meta.loc.clone(),
                )
                .with_hint(format!("Expected: {expected}"))
                .with_hint(format!("Found   : {input}"))
                .with_hint(format!(
                    "Type `{}` is incompatible with `{}`",
                    expected_ty, incompatible_ty
                ))
            }
            (
                ConstraintOrigin::OutputSubtyping { .. },
                InferenceErrorKind::StacksDiffer { expected, found },
            ) if expected.len() != found.len() => {
                let expected = self.format_stack(&expected, '[', ']');
                let found = self.format_stack(&found, '[', ']');

                HayError::new(
                    format!("Wrong number of elements left on the stack at end of function",),
                    e.meta.loc.clone(),
                )
                .with_hint(format!("Expected: {expected}"))
                .with_hint(format!("Found   : {found}"))
            }
            (
                ConstraintOrigin::OutputSubtyping { expected, found },
                InferenceErrorKind::TypesIncompatible {
                    expected: expected_tid,
                    found: found_tid,
                },
            ) if expected.len() == found.len() => {
                let expected = self.format_stack(&expected, '[', ']');
                let found = self.format_stack(&found, '[', ']');

                let expected_ty = self.format_tid(expected_tid);
                let incompatible_ty = self.format_tid(found_tid);

                HayError::new(
                    format!("Incompatible Stacks at end of function"),
                    e.meta.loc.clone(),
                )
                .with_hint(format!("Expected: {expected}"))
                .with_hint(format!("Found   : {found}"))
                .with_hint(format!(
                    "Type `{}` is incompatible with `{}`",
                    expected_ty, incompatible_ty
                ))
            }
            (
                ConstraintOrigin::OutputSubtyping { expected, found },
                InferenceErrorKind::TypesIncompatible {
                    expected: expected_tid,
                    found: found_tid,
                },
            ) if expected.len() != found.len() => {
                let diff = (expected.len() as isize) - (found.len() as isize);

                let expected = self.format_stack(&expected, '[', ']');
                let found = self.format_stack(&found, '[', ']');

                let expected_ty = self.format_tid(expected_tid);
                let incompatible_ty = self.format_tid(found_tid);

                let e = HayError::new(
                    format!("Incompatible Stacks at end of function"),
                    e.meta.loc.clone(),
                )
                .with_hint(format!("Expected: {expected}"))
                .with_hint(format!("Found   : {found}"));

                if diff > 0 {
                    e.with_hint(format!(
                        "Expected {diff} additional element{} on the stack",
                        if diff > 1 { "s" } else { "" }
                    ))
                } else {
                    e.with_hint(format!(
                        "Found {} extra element{} on the stack",
                        diff.abs(),
                        if diff < -1 { "s" } else { "" },
                    ))
                }
            }
            (ConstraintOrigin::UnboundVarInBody, InferenceErrorKind::UnboundVar(tyvar)) => {
                let tid = self.typestore.intern(Type::TypeVar(tyvar));
                match self.typestore.tyvar_origin.get(&tid).cloned() {
                    Some((tid, token)) => HayError::new(
                        format!("Cannot infer type for `{}`", self.format_tid(tid)),
                        token.loc,
                    )
                    .with_hint(format!(
                        "Consider providing a type hint:\n            `{} #u64 hint`",
                        token.lexeme
                    )),
                    None => todo!("{:?}", self.symbols.types.get(&tid)),
                }
            }
            (
                ConstraintOrigin::ConflictingImpls,
                InferenceErrorKind::ConflictingImpls { cid, a, b },
            ) => {
                let iface_name = self.symbols.interfaces.get(&cid).unwrap().clone();
                HayError::new(
                    format!(
                        "Conflicting implementations for interface `{}`",
                        iface_name.lexeme
                    ),
                    e.meta.loc.clone(),
                )
                .with_hint(format!(
                    "`{}{}` conflicts with\n            `{}{}`",
                    iface_name.lexeme,
                    self.format_stack(&b.args, '<', '>'),
                    iface_name.lexeme,
                    self.format_stack(&a.args, '<', '>')
                ))
                .with_hint("Overlapping implementations cannot override default functions")
            }
            (
                ConstraintOrigin::ImplsMustMatch,
                InferenceErrorKind::TypesIncompatible { expected, found },
            ) => HayError::new(
                format!("Incompatible function signature for `{}`", e.meta.lexeme),
                e.meta.loc,
            )
            .with_hint(format!("Expected: {}", self.format_tid(expected)))
            .with_hint(format!("Found   : {}", self.format_tid(found))),
            (
                ConstraintOrigin::Branching {
                    fresh_stack,
                    stacks,
                },
                InferenceErrorKind::TypesIncompatible { expected, found },
            ) => {
                let mut e = HayError::new("Branches must produce similar stacks.", e.meta.loc);
                for (i, stack) in stacks.into_iter().enumerate() {
                    e = e.with_hint(format!(
                        "Branch {}: {}",
                        i + 1,
                        self.format_stack_state(&stack, '[', ']')
                    ))
                }
                e = e.with_hint(format!(
                    "No common type can be found between `{}` and `{}`",
                    self.format_tid(expected),
                    self.format_tid(found),
                ));

                e
            }
            (
                ConstraintOrigin::FunctionCall { input, expected },
                InferenceErrorKind::TypeParameterConstrained { var, tid },
            ) => {
                let var = self.typestore.intern(Type::TypeVar(var));

                HayError::new(
                    format!(
                        "Type parameter `{}` constrained during call to `{}`",
                        self.format_tid(var),
                        e.meta.lexeme
                    ),
                    e.meta.loc,
                )
                .with_hint(format!(
                    "Expected: {}",
                    self.format_stack(&expected, '[', ']')
                ))
                .with_hint(format!("Found   : {}", self.format_stack(&input, '[', ']')))
            }
            (
                ConstraintOrigin::FunctionCall { input, expected },
                InferenceErrorKind::PredicateNotMet { cid, args },
            ) => HayError::new(
                format!(
                    "Can't call `{}` because predicate `{}` is not satisfied.",
                    e.meta.lexeme,
                    self.format_predicate(cid, args)
                ),
                e.meta.loc,
            ),
            (
                ConstraintOrigin::FunctionCall { .. },
                InferenceErrorKind::NotEnoughArgs { expected, stack },
            ) => HayError::new(
                format!("Too few arguments for `{}`.", e.meta.lexeme),
                e.meta.loc,
            )
            .with_hint(format!(
                "Expected: {}",
                self.format_stack(&expected, '[', ']')
            ))
            .with_hint(format!("Found   : {}", self.format_stack(&stack, '[', ']'))),
            (
                ConstraintOrigin::TypeInstance(tid),
                InferenceErrorKind::WrongNumberOfArgs { expected, found },
            ) => HayError::new(
                format!(
                    "Wrong number of type parameters for type `{}`",
                    self.format_tid(tid)
                ),
                e.meta.loc.clone(),
            )
            .with_hint(format!(
                "Expected: {}",
                self.format_stack(&expected, '<', '>')
            ))
            .with_hint(format!("Found: {}", self.format_stack(&found, '<', '>'))),
            (
                ConstraintOrigin::NamedMemberAccess(tid),
                InferenceErrorKind::InvalidStructMember { member },
            ) => HayError::new(
                format!(
                    "`{}` doesn't have a member `{member}`",
                    self.format_tid(tid)
                ),
                e.meta.loc.clone(),
            ),
            (ConstraintOrigin::OutputUnreachable, InferenceErrorKind::UnreachableStateReached { stack }) => {
                HayError::new(format!("Reached end of function `{}` which shouldn't return", e.meta.lexeme), e.meta.loc.clone())
            }
            (origin, kind) => todo!("Generate Error message for {origin:?}, {kind:?}"),
        }
    }

    fn add_params_to_scope(
        &mut self,
        params: &[ParseType<'src>],
        token: &Token<'src>,
    ) -> Result<Vec<TypeVar>, HayError> {
        let mut tyvars = vec![];
        for ty in params {
            tyvars.push(self.insert_param_ty(ty)?);
        }

        self.bindings.insert(token.clone(), tyvars.clone());

        Ok(tyvars)
    }

    fn create_scheme(
        &mut self,
        params: &[ParseType<'src>],
        inputs: &[Argument<'src>],
        outputs: &Option<Vec<ParseType<'src>>>,
        predicates: &[TypePredicate],
        token: &Token<'src>,
    ) -> Result<Scheme, HayError> {
        let mut tyvar_params = self.add_params_to_scope(params, token)?;

        let mut input = vec![];
        for arg in inputs {
            let tid = self.get_tid(&arg.typ)?;
            input.push(tid);
        }

        let output = match outputs {
            Some(outputs) => {
                let mut output = vec![];
                for ty in outputs {
                    let tid = self.get_tid(&ty)?;
                    output.push(tid);
                }
                StackState::Reachable(output)
            }
            None => StackState::Unreachable,
        };

        let tid = self.typestore.function(FunctionType {
            input: input.clone(),
            output: output,
        });

        Ok(Scheme::new(tyvar_params, tid, predicates))
    }

    fn bind_function_def(&mut self, func: &FunctionDefinition<'src>) -> Result<(), HayError> {
        // insert this function into the global scope
        let var = self.insert_var(&func.name, Scoping::Global)?;
        self.scope.new_scope();
        let scheme = self.create_scheme(
            &func.params,
            &func.signature.input,
            &func.signature.output,
            &[],
            &func.name,
        )?;
        self.scope.end_scope();
        if let Some(_) = self.env.bind_scheme(var, scheme) {
            todo!("repeat binding of scheme")
        }

        Ok(())
    }

    fn bind_interface_function_def(
        &mut self,
        func: &InterfaceFunctionDefinition<'src>,
        predicate: &TypePredicate,
        mut parent_vars: Vec<TypeVar>,
    ) -> Result<(), HayError> {
        // insert this function into the global scope
        let var = self.insert_var(&func.name, Scoping::Global)?;

        self.scope.new_scope();
        let mut scheme = self.create_scheme(
            &func.params,
            &func.signature.input,
            &func.signature.output,
            &[predicate.clone()],
            &func.name,
        )?;

        parent_vars.extend(scheme.vars);
        scheme.vars = parent_vars;
        self.scope.end_scope();
        if let Some(_) = self.env.bind_scheme(var, scheme) {
            todo!("repeat binding of scheme")
        }

        Ok(())
    }

    fn desugar_bindings(
        &mut self,
        token: &Token<'src>,
        inputs: &[Argument<'src>],
        expr: &ParseExpr<'src>,
    ) -> Result<Expr<'src, Token<'src>>, HayError> {
        let mut binds = vec![];
        for arg in inputs {
            if let Some(ident) = &arg.ident {
                let var = self.insert_var(ident, Scoping::Local)?;
                binds.push(var);
            }
        }

        let body = self.lower_expr(&expr)?;
        let expr = Expr::block([Expr::bind(binds, token.clone()), body], token.clone());

        Ok(expr)
    }

    fn create_function(
        &self,
        name: &'src str,
        expr: Expr<'src, Token<'src>>,
    ) -> Function<'src, Token<'src>> {
        let var = self.scope.get_var(name).unwrap().0;
        let scheme = self.env.get_var(&var).unwrap().clone();
        let Type::Function(f) = self.typestore.get(scheme.tid).cloned().unwrap() else {
            unreachable!()
        };
        Function::new(f.input, f.output, scheme.vars, scheme.predicates, expr)
    }

    fn rebind_params(
        &mut self,
        token: &Token<'src>,
        params: &[ParseType<'src>],
    ) -> Result<Vec<TypeVar>, HayError> {
        let Some(tyvars) = self.bindings.get(token).cloned() else {
            unreachable!("{token}: Params should have been bound to their origin");
        };

        assert!(tyvars.len() == params.len());
        for (tyvar, ty) in tyvars.iter().zip(params.iter()) {
            self.insert_tyvar_to_scope(*tyvar, ty.token())?;
        }

        Ok(tyvars)
    }

    fn type_check_function_def(
        &mut self,
        func: &FunctionDefinition<'src>,
    ) -> Result<(Var, TypedFunction<'src>), HayError> {
        self.scope.new_scope();
        let vars = self.bindings.get(&func.name).cloned().unwrap();

        let params = self.rebind_params(&func.name, &func.params)?;

        let expr = self.desugar_bindings(&func.name, &func.signature.input, &func.expr)?;
        let f = self
            .create_function(&func.name.lexeme, expr)
            .type_check(&mut self.typestore, &self.env, &self.scope)
            .map_err(|e| self.map_inference_err(e))?;
        self.scope.end_scope();

        let (var, _) = self.scope.get_var(&func.name.lexeme).unwrap();

        Ok((var, f))
    }

    fn new() -> Self {
        let mut typestore = TypeStore::new();
        let symbols = SymbolTable::new(&mut typestore);
        let scope = Scope::new(&mut typestore);
        let env = Environment::new();
        let bindings = HashMap::new();
        Self {
            typestore,
            env,
            symbols,
            scope,
            bindings,
        }
    }

    fn bind_struct_def(&mut self, s: &StructDefinition<'src>) -> Result<(), HayError> {
        // Only have the vars known within the scope of the member definition.
        self.scope.new_scope();
        let vars = self.add_params_to_scope(&s.params, &s.name)?;
        let members = s
            .members
            .iter()
            .map(|(ty, _)| self.get_tid(ty))
            .collect::<Result<Vec<_>, _>>()?;
        let def = StructDef::new(&self.typestore, vars, members, &s.name)
            .map_err(|e| self.map_inference_err(e))?;
        let (tid, sid) = self.typestore.add_struct(def);

        if s.name.lexeme == "Str" {
            self.typestore.set_str_tid(tid);
        }

        self.scope.end_scope();

        // Add the struct to the global scope.
        if let Some(_) = self
            .scope
            .insert_tid(s.name.lexeme.into(), tid, s.name.clone())
        {
            todo!("Error type already defined")
        }
        if let Some(_) = self.symbols.insert_tid(
            tid,
            TypeInfo {
                name: s.name.lexeme.into(),
                token: Some(s.name.clone()),
            },
        ) {
            todo!()
        }

        for (i, (_, ident)) in s.members.iter().enumerate() {
            if let Some(_) = self.scope.insert_member(sid, ident.lexeme, i) {
                todo!("Duplicate member idents")
            }
            if let Some(_) = self.symbols.insert_member(sid, i, ident.clone()) {
                todo!("Duplicate member idents")
            }
        }

        if let Some(_) = self.symbols.insert_struct(sid, s.name.clone()) {
            todo!()
        }

        Ok(())
    }

    fn bind_enum_def(&mut self, e: &EnumDefinition<'src>) -> Result<(), HayError> {
        // Only have the vars known within the scope of the member definition.
        self.scope.new_scope();
        let vars = self.add_params_to_scope(&e.params, &e.name)?;

        let mut variants = vec![];
        for (ty, _) in &e.variants {
            let v = if let Some(ty) = ty {
                Some(self.get_tid(ty)?)
            } else {
                None
            };
            variants.push(v)
        }

        let def = EnumDef::new(&self.typestore, vars, variants, &e.name)
            .map_err(|e| self.map_inference_err(e))?;
        let (tid, eid) = self.typestore.add_enum(def);

        self.scope.end_scope();

        // Add the struct to the global scope.
        if let Some(_) = self
            .scope
            .insert_tid(e.name.lexeme.into(), tid, e.name.clone())
        {
            todo!("Error type already defined")
        }

        if let Some(_) = self.symbols.insert_tid(
            tid,
            TypeInfo {
                name: e.name.lexeme.into(),
                token: Some(e.name.clone()),
            },
        ) {
            todo!()
        }

        for (i, (_, ident)) in e.variants.iter().enumerate() {
            if let Some(_) = self.scope.insert_variant(eid, ident.lexeme, i) {
                todo!("Duplicate variant idents")
            }
            if let Some(_) = self.symbols.insert_variant(eid, i, ident.clone()) {
                todo!("Duplicate variant idents")
            }
        }

        self.symbols.enums.insert(eid, e.name.clone());

        Ok(())
    }

    fn create_interface(
        &mut self,
        iface: &InterfaceDefinition<'src>,
    ) -> Result<(Vec<TypeVar>, Vec<TypeId>, TypeClassId), HayError> {
        let vars = self.add_params_to_scope(&iface.params, &iface.name)?;
        let var_tids: Vec<_> = vars
            .iter()
            .map(|tyvar| self.typestore.intern(Type::TypeVar(*tyvar)))
            .collect();

        let interface = Interface {
            vars: vars.clone(),
            associated_types_count: iface.associated_types.len(),
            coherence: if iface.funcs.iter().all(|f| f.expr.is_some())
                && iface.associated_types.len() == 0
            {
                Coherence::Overlapping
            } else {
                Coherence::Unique
            },
        };

        Ok((vars, var_tids, self.env.add_typeclass(interface)))
    }

    fn create_associated_type_name_list(
        &mut self,
        associated_types: &[Token<'src>],
        cid: TypeClassId,
        var_tids: &Vec<TypeId>,
    ) -> Vec<&'src str> {
        let mut type_names = vec![];
        for (idx, name) in associated_types.iter().enumerate() {
            type_names.push(name.lexeme);
            let p = Projection {
                cid,
                args: var_tids.clone(),
                idx,
            };

            let tid = self.typestore.intern(Type::Projection(p));
            if let Some(_) = self.scope.insert_tid(name.lexeme.into(), tid, name.clone()) {
                todo!("")
            }
        }

        type_names
    }

    fn create_interface_fn_lookup(
        &mut self,
        funcs: &[InterfaceFunctionDefinition<'src>],
        predicates: &[TypePredicate],
    ) -> Result<InterfaceFnsLookup<'src>, HayError> {
        let mut fn_lookup = InterfaceFnsLookup::new();
        for func in funcs {
            self.scope.new_scope();
            self.rebind_params(&func.name, &func.params);
            let tid = self.get_function_tid(&func.signature)?;
            if fn_lookup
                .insert(
                    func.name.lexeme,
                    InterfaceFnInfo {
                        token: func.name.clone(),
                        tid,
                        params: func.params.clone(),
                        predicates: predicates.to_vec(),
                        body: None,
                    },
                )
                .is_some()
            {
                todo!("re-definition of interface function")
            }
            self.scope.end_scope();
        }
        Ok(fn_lookup)
    }

    fn bind_interface_def(&mut self, iface: &InterfaceDefinition<'src>) -> Result<(), HayError> {
        // Only have the vars known within the scope of the member definition.
        self.scope.new_scope();

        let (vars, var_tids, cid) = self.create_interface(iface)?;
        if self.symbols.insert_cid(cid, iface.name.clone()).is_some() {
            todo!("Error interface of this cid already exists")
        }

        let predicate = TypePredicate {
            cid,
            args: var_tids.clone(),
        };

        let associated_types =
            self.create_associated_type_name_list(&iface.associated_types, cid, &var_tids);
        for func in &iface.funcs {
            self.bind_interface_function_def(func, &predicate, vars.clone())?;
        }
        let fn_lookup = self.create_interface_fn_lookup(&iface.funcs, &[predicate])?;

        self.scope.end_scope();

        if let Some(_) = self.scope.insert_cid(
            iface.name.lexeme,
            InterfaceInfo {
                cid,
                fns: fn_lookup,
                associated_types,
            },
        ) {
            todo!("error interface of this name already exists")
        }

        Ok(())
    }

    fn type_check_interface_def(
        &mut self,
        iface: &InterfaceDefinition<'src>,
    ) -> Result<(Vec<(Var, Option<TypedFunction<'src>>)>, TypeClassId), HayError> {
        let mut iface_info = self.scope.get_cid(&iface.name.lexeme).cloned().unwrap();

        self.scope.new_scope();
        let parent_params = self.rebind_params(&iface.name, &iface.params)?;

        let mut default_impls = vec![];

        for func in &iface.funcs {
            self.scope.new_scope();

            let mut params = self.rebind_params(&func.name, &func.params)?;
            params = parent_params
                .clone()
                .into_iter()
                .chain(params.into_iter())
                .collect();
            let (var, _) = self.scope.get_var(&func.name.lexeme).unwrap();
            if let Some(expr) = &func.expr {
                let predicates = self.env.get_var(&var).unwrap().predicates.clone();
                let (expr, typed_func) = self.type_check_function(
                    &func.name,
                    &func.signature,
                    &params,
                    &predicates,
                    &expr,
                )?;

                iface_info.fns.get_mut(&func.name.lexeme).unwrap().body = Some(expr);

                default_impls.push((var, Some(typed_func)));
            } else {
                default_impls.push((var, None));
            }

            self.scope.end_scope();
        }
        self.scope.end_scope();
        let cid = iface_info.cid;
        self.scope.insert_cid(&iface.name.lexeme, iface_info);

        Ok((default_impls, cid))
    }

    fn create_predicate(&mut self, ty: &ParseType<'src>) -> Result<TypePredicate, HayError> {
        match ty {
            ParseType::Simple(ident) => {
                let mut args = vec![];
                for ty in &ident.params {
                    args.push(self.get_tid(ty)?)
                }

                let Some(iface_info) = self.scope.get_cid(&ident.base.lexeme) else {
                    todo!("Not an interface");
                };

                Ok(TypePredicate {
                    cid: iface_info.cid,
                    args,
                })
            }
            _ => todo!("Not an interface"),
        }
    }

    fn create_predicates(
        &mut self,
        tys: &[ParseType<'src>],
    ) -> Result<Vec<TypePredicate>, HayError> {
        let mut predicates = vec![];
        for ty in tys {
            predicates.push(self.create_predicate(ty)?);
        }

        Ok(predicates)
    }

    fn create_interface_impl(
        &mut self,
        iface_impl: &ParseInterfaceImpl<'src>,
        info: &InterfaceInfo<'src>,
    ) -> Result<InterfaceImpl, HayError> {
        let vars = self.add_params_to_scope(&iface_impl.params, &iface_impl.name)?;
        let mut args = vec![];
        for ty in &iface_impl.args {
            args.push(self.get_tid(ty)?)
        }

        let predicates = self.create_predicates(&iface_impl.predicates)?;
        let mut associated_types = vec![];
        for type_name in &info.associated_types {
            match iface_impl
                .associated_types
                .iter()
                .find(|(name, _)| name.lexeme == *type_name)
            {
                Some((_, ty)) => associated_types.push(self.get_tid(ty)?),
                None => todo!("Error: Unexpected associated type"),
            }
        }

        let coherence = if iface_impl.impls.is_empty() && associated_types.len() == 0 {
            Coherence::Overlapping
        } else {
            Coherence::Unique
        };

        let interface_impl = InterfaceImpl {
            cid: info.cid,
            vars,
            args,
            associated_types,
            predicates,
            coherence,
        };

        Ok(interface_impl)
    }

    fn bind_interface_impl(
        &mut self,
        iface_impl: &ParseInterfaceImpl<'src>,
    ) -> Result<(), HayError> {
        let Some(mut info) = self.scope.get_cid(iface_impl.name.lexeme).cloned() else {
            todo!()
        };

        self.scope.new_scope();
        let interface_impl = self.create_interface_impl(iface_impl, &info)?;
        self.scope.end_scope();

        self.env
            .add_impl(
                &mut self.typestore,
                info.cid,
                interface_impl,
                &iface_impl.name,
            )
            .map_err(|e| self.map_inference_err(e))?;

        Ok(())
    }

    fn type_check_function(
        &mut self,
        name: &Token<'src>,
        signature: &Signature<'src>,
        params: &[TypeVar],
        predicates: &[TypePredicate],
        expr: &ParseExpr<'src>,
    ) -> Result<(Expr<'src, Token<'src>>, TypedFunction<'src>), HayError> {
        let tid = self.get_function_tid(&signature)?;
        let Type::Function(f) = self.typestore.get(tid).unwrap().clone() else {
            unreachable!();
        };
        let expr = self.desugar_bindings(&name, &signature.input, &expr)?;

        let f = Function::new(f.input, f.output, params, predicates, expr.clone())
            .type_check(&mut self.typestore, &self.env, &self.scope)
            .map_err(|e| self.map_inference_err(e))?;

        Ok((expr, f))
    }

    fn create_interface_replacements(
        &mut self,
        name: &'src str,
        args: Vec<TypeId>,
    ) -> Result<HashMap<TypeVar, TypeId>, HayError> {
        let Some((var, origin)) = self.scope.get_var(&name) else {
            todo!()
        };
        let Some(scheme) = self.env.get_var(&var) else {
            todo!()
        };

        if (scheme.vars.len() != args.len()) {
            todo!()
        }

        Ok(HashMap::from_iter(
            scheme.vars.clone().into_iter().zip(args.into_iter()),
        ))
    }

    fn validate_interface_function_signature(
        &mut self,
        token: &Token<'src>,
        expected_tid: TypeId,
        tid: TypeId,
        replacements: &HashMap<TypeVar, TypeId>,
    ) -> Result<(), HayError> {
        let expected_tid = self.typestore.replace(expected_tid, &replacements);
        let expected_tid = self
            .typestore
            .normalize_ty(&mut self.env, expected_tid, token)
            .map_err(|_| todo!())?
            .1;
        let tid = self
            .typestore
            .normalize_ty(&mut self.env, tid, token)
            .map_err(|_| todo!())?
            .1;

        if expected_tid != tid {
            return Err(HayError::new(
                format!(
                    "Incompatible function signature for interface function `{}`",
                    token.lexeme
                ),
                token.loc.clone(),
            )
            // .with_hint(format!(
            //     "`{}` defined here: {}",
            //     func.name.lexeme,
            //     origin.loc.clone()
            // ))
            .with_hint(format!("Expected: {}", self.format_tid(expected_tid)))
            .with_hint(format!("Found   : {}", self.format_tid(tid))));
            todo!()
        }

        Ok(())
    }

    fn type_check_interface_impl(
        &mut self,
        iface_impl: &ParseInterfaceImpl<'src>,
    ) -> Result<
        (
            TypeClassId,
            TypedInterfaceImpl,
            Vec<(Var, TypedFunction<'src>)>,
        ),
        HayError,
    > {
        let info: InterfaceInfo<'_> = self
            .scope
            .get_cid(&iface_impl.name.lexeme)
            .cloned()
            .unwrap();

        self.scope.new_scope();
        let parent_params = self.rebind_params(&iface_impl.name, &iface_impl.params)?;
        self.typestore
            .solidify_vars(&parent_params)
            .map_err(|_| todo!())?;
        let mut parent_args = vec![];
        for ty in &iface_impl.args {
            parent_args.push(self.get_tid(ty)?);
        }

        let parent_predicates = self.create_predicates(&iface_impl.predicates)?;
        let mut impls = HashMap::new();
        for (name, fn_info) in info.fns {
            self.scope.new_scope();

            let mut params = self.add_params_to_scope(&fn_info.params, &fn_info.token)?;
            let mut args = vec![];
            for ty in &fn_info.params {
                args.push(self.get_tid(ty)?);
            }
            let args = parent_args
                .clone()
                .into_iter()
                .chain(args.into_iter())
                .collect();

            let replacements = self.create_interface_replacements(name, args)?;
            let tid = self.typestore.replace(fn_info.tid, &replacements);

            impls.insert(
                name,
                InterfaceFnInfo {
                    token: fn_info.token,
                    tid,
                    params: fn_info.params,
                    predicates: parent_predicates
                        .clone()
                        .into_iter()
                        .chain(fn_info.predicates.into_iter())
                        .collect(),
                    body: fn_info.body,
                },
            );

            self.scope.end_scope();
        }

        for fn_impl in &iface_impl.impls {
            self.scope.new_scope();

            self.add_params_to_scope(&fn_impl.params, &fn_impl.name)?;

            let mut args = vec![];
            for ty in &fn_impl.params {
                args.push(self.get_tid(ty)?);
            }
            let args = parent_args
                .clone()
                .into_iter()
                .chain(args.into_iter())
                .collect();
            let replacements = self.create_interface_replacements(&fn_impl.name.lexeme, args)?;
            let tid = self.get_function_tid(&fn_impl.signature)?;
            let Some((var, _)) = self.scope.get_var(&fn_impl.name.lexeme) else {
                todo!()
            };

            let Some(Scheme {
                tid: expected_tid, ..
            }) = self.env.get_var(&var)
            else {
                todo!()
            };
            self.validate_interface_function_signature(
                &fn_impl.name,
                *expected_tid,
                tid,
                &replacements,
            )?;

            let expr =
                self.desugar_bindings(&fn_impl.name, &fn_impl.signature.input, &fn_impl.expr)?;

            assert!(impls
                .insert(
                    fn_impl.name.lexeme,
                    InterfaceFnInfo {
                        token: fn_impl.name.clone(),
                        tid,
                        params: fn_impl.params.clone(),
                        predicates: parent_predicates.clone(),
                        body: Some(expr)
                    }
                )
                .is_some());

            self.scope.end_scope();
        }

        let mut typed_functions = vec![];
        let mut typed_impl = TypedInterfaceImpl {
            args: parent_args,
            associated_types: info
                .associated_types
                .iter()
                .map(|type_name| {
                    match iface_impl
                        .associated_types
                        .iter()
                        .find(|(name, _)| name.lexeme == *type_name)
                    {
                        Some((_, ty)) => self.get_tid(ty),
                        None => todo!("Error: Unexpected associated type"),
                    }
                })
                .collect::<Result<_, _>>()?,

            fn_map: HashMap::new(),
        };

        for (name, fn_info) in impls {
            self.scope.new_scope();
            let mut params = self.rebind_params(&fn_info.token, &fn_info.params)?;

            params = parent_params
                .clone()
                .into_iter()
                .chain(params.into_iter())
                .collect();

            let Type::Function(f) = self.typestore.get(fn_info.tid).unwrap().clone() else {
                unreachable!();
            };

            let f = Function::new(
                f.input,
                f.output,
                params.clone(),
                fn_info.predicates.clone(),
                fn_info.body.unwrap(),
            )
            .type_check(&mut self.typestore, &self.env, &self.scope)
            .map_err(|e| self.map_inference_err(e))
            .unwrap();

            let (var, _) = self.scope.get_var(name).unwrap();
            let fresh_var = self.env.fresh_var();
            let scheme = Scheme::new(params, f.expr.meta.1, fn_info.predicates);
            self.env.bind_scheme(fresh_var, scheme);

            typed_impl.fn_map.insert(var, fresh_var);
            typed_functions.push((fresh_var, f));

            self.scope.end_scope();
        }

        self.scope.end_scope();

        // typed_functions
        //     .iter_mut()
        //     .for_each(|(_, expr)| expr.expr.map_vars(&typed_impl.fn_map));

        Ok((info.cid, typed_impl, typed_functions))
    }

    fn bind_function_declaration(
        &mut self,
        func: &FunctionDeclaration<'src>,
    ) -> Result<(Var, &'src str, TypeId), HayError> {
        let var = self.insert_var(&func.name, Scoping::Global)?;
        let tid = self.get_function_tid(&func.signature)?;

        let scheme = Scheme {
            vars: vec![],
            tid,
            predicates: vec![],
        };

        self.env.bind_scheme(var, scheme);

        Ok((var, &func.name.lexeme, tid))
    }

    fn create_bindings(
        &mut self,
        statements: &[Statememt<'src>],
    ) -> Result<Vec<(Var, &'src str, TypeId)>, HayError> {
        let mut externs = vec![];
        for statement in statements {
            match statement {
                Statememt::StructDefinition(s) => self.bind_struct_def(s)?,
                Statememt::EnumDefinition(e) => self.bind_enum_def(e)?,
                Statememt::FunctionDefinition(func) => self.bind_function_def(func)?,
                Statememt::InterfaceDefinition(iface) => self.bind_interface_def(iface)?,
                Statememt::InterfaceImpl(iface_impl) => self.bind_interface_impl(iface_impl)?,
                Statememt::ExternFunctionDeclaratoin(func) => {
                    externs.push(self.bind_function_declaration(func)?);
                }
            }
        }

        Ok(externs)
    }

    fn type_check_statements(
        &mut self,
        statements: &[Statememt<'src>],
        world: &mut TypedWorld<'src>,
    ) -> Result<(), HayError> {
        for statement in statements {
            match statement {
                Statememt::StructDefinition(s) => (),
                Statememt::EnumDefinition(e) => (),
                Statememt::FunctionDefinition(func) => {
                    let (var, func) = self.type_check_function_def(func)?;
                    world.add_func(var, func);
                }
                Statememt::InterfaceDefinition(iface) => {
                    let (impls, cid) = self.type_check_interface_def(iface)?;
                    for (var, func) in impls {
                        world.interface_fns.insert(var, cid);
                        if let Some(func) = func {
                            world.add_func(var, func);
                        }
                    }
                }
                Statememt::InterfaceImpl(iface_impl) => {
                    let (cid, typed_impl, impls) = self.type_check_interface_impl(iface_impl)?;
                    world.add_impl(cid, typed_impl);
                    for (var, func) in impls {
                        world.add_func(var, func);
                    }
                }
                Statememt::ExternFunctionDeclaratoin(func) => (),
            }
        }

        Ok(())
    }

    pub fn build_world(
        mut statements: Vec<Statememt<'src>>,
    ) -> Result<
        (
            TypedWorld<'src>,
            TypeStore<Token<'src>>,
            Environment,
            Vec<(Var, &'src str, TypeId)>,
            Option<Var>,
        ),
        HayError,
    > {
        let mut tc = TypeCheck::new();
        let mut world = TypedWorld::default();
        let externs = tc.create_bindings(&statements)?;
        tc.type_check_statements(&statements, &mut world)?;

        let entry = tc.scope.get_var("main").map(|(var, _)| var);

        Ok((world, tc.typestore, tc.env, externs, entry))
    }
}

#[derive(Default)]
pub struct TypedInterfaceImpl {
    args: Vec<TypeId>,
    associated_types: Vec<TypeId>,
    fn_map: HashMap<Var, Var>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FunctionInstance {
    var: Var,
    args: Vec<TypeId>,
}

struct MonoContext<'a, 'src> {
    world: &'a TypedWorld<'src>,
    env: &'a mut Environment,
    typestore: &'a mut TypeStore<Token<'src>>,

    // Each concrete specialization gets a unique Var.
    instances: HashMap<FunctionInstance, Var>,

    // Instances waiting to be processed.
    pending: Vec<FunctionInstance>,

    // Monomorphized functions.
    functions: Vec<(Var, TypedFunction<'src>)>,

    // Concrete types required by the program.
    types: HashSet<TypeId>,
}

impl<'a, 'src> MonoContext<'a, 'src> {
    fn new(
        world: &'a TypedWorld<'src>,
        env: &'a mut Environment,
        typestore: &'a mut TypeStore<Token<'src>>,
    ) -> Self {
        Self {
            world,
            env,
            typestore,
            instances: HashMap::new(),
            pending: vec![],
            functions: vec![],
            types: HashSet::new(),
        }
    }

    fn request_type_inner(
        &mut self,
        tid: TypeId,
        substitutions: &HashMap<TypeVar, TypeId>,
    ) -> HashSet<TypeId> {
        let tid = self.typestore.replace(tid, substitutions);
        if self.types.contains(&tid) {
            return HashSet::new();
        }

        let mut concrete = HashSet::new();
        if !self.typestore.is_free(&tid) {
            concrete.insert(tid);

            match &self.typestore.get(tid).unwrap().clone() {
                Type::TypeVar(_) | Type::RigidTypeVar(_) => {
                    concrete.insert(tid);
                }
                Type::Builtin(_) => (),
                Type::Function(f) => {
                    f.input.iter().for_each(|tid| {
                        concrete.extend(self.request_type_inner(*tid, substitutions))
                    });
                    match &f.output {
                        StackState::Reachable(output) => output.iter().for_each(|tid| {
                            concrete.extend(self.request_type_inner(*tid, substitutions))
                        }),
                        StackState::Unreachable => (),
                    }
                }
                Type::Pointer(pointer_type) => {
                    concrete.extend(self.request_type_inner(pointer_type.tid, substitutions));
                }
                Type::Struct(s) => {
                    let s = s.clone();
                    let args: Vec<_> = s
                        .args
                        .iter()
                        .map(|tid| {
                            concrete.extend(self.request_type_inner(*tid, substitutions));
                            self.typestore.replace(*tid, substitutions)
                        })
                        .collect();

                    let members = self.typestore.get_struct_members(s.clone());
                    members.into_iter().for_each(|tid| {
                        concrete.extend(self.request_type_inner(tid, &substitutions))
                    });
                }
                Type::Enum(e) => {
                    let args: Vec<_> = e
                        .args
                        .iter()
                        .map(|tid| {
                            concrete.extend(self.request_type_inner(*tid, substitutions));
                            self.typestore.replace(*tid, substitutions)
                        })
                        .collect();
                    let variants = self.typestore.get_enum_variants(e.eid, e.args.clone());
                    variants.into_iter().for_each(|tid| {
                        if let Some(tid) = tid {
                            concrete.extend(self.request_type_inner(tid, &substitutions))
                        }
                    });
                }
                Type::Tuple(tids) => tids
                    .iter()
                    .for_each(|tid| concrete.extend(self.request_type_inner(*tid, substitutions))),
                Type::Variant(v) => {
                    v.args.iter().for_each(|tid| {
                        concrete.extend(self.request_type_inner(*tid, substitutions))
                    });

                    let variants = self.typestore.get_enum_def(v.eid).variants.clone();
                    variants.into_iter().for_each(|maybe_tid| {
                        maybe_tid.map(|tid| {
                            concrete.extend(self.request_type_inner(tid, substitutions))
                        });
                    });
                }
                Type::Type(tid) => {
                    concrete.extend(self.request_type_inner(*tid, substitutions));
                }
                Type::Projection(projection) => projection
                    .args
                    .iter()
                    .for_each(|tid| concrete.extend(self.request_type_inner(*tid, substitutions))),
            };
        };

        concrete
    }

    fn request_type(&mut self, tid: TypeId, substitutions: &HashMap<TypeVar, TypeId>) -> TypeId {
        let tid = self.typestore.replace(tid, substitutions);

        let new_types = self.request_type_inner(tid, substitutions);
        self.types.extend(new_types);

        tid
    }

    fn request_function(&mut self, instance: FunctionInstance) -> Var {
        // Already requested or generated.
        if let Some(var) = self.instances.get(&instance) {
            return *var;
        }

        // Allocate a new function identity.
        let mono_var = self.env.fresh_var();

        // Register BEFORE processing its body.
        self.instances.insert(instance.clone(), mono_var);

        // Schedule the specialization.
        self.pending.push(instance);

        mono_var
    }

    fn monomorphize_function(&mut self, instance: FunctionInstance) {
        let original = &self.world.functions[&instance.var];
        let mono_var = self.instances[&instance];

        assert_eq!(original.params.len(), instance.args.len());

        let substitutions = HashMap::from_iter(
            original
                .params
                .iter()
                .zip(instance.args.iter())
                .map(|(var, tid)| (*var, *tid)),
        );

        let tid = self.request_type(original.expr.meta.1, &substitutions);

        // Specialize the function body.
        let expr = self.monomorphize_expr(original.expr.clone(), &substitutions);

        let Type::Function(FunctionType { input, output }) =
            self.typestore.get(tid).cloned().unwrap()
        else {
            unreachable!()
        };

        let func = Function::new(input, output, vec![], vec![], expr);

        self.functions.push((mono_var, func));
    }

    fn monomorphize_pattern(
        &mut self,
        pattern: MatchPattern,
        replacements: &HashMap<TypeVar, TypeId>,
    ) -> MatchPattern {
        match pattern {
            MatchPattern::Variant(tid) => {
                MatchPattern::Variant(self.request_type(tid, replacements))
            }
        }
    }

    fn monomorphize_expr(
        &mut self,
        expr: Expr<'src, (Token<'src>, TypeId)>,
        substitutions: &HashMap<TypeVar, TypeId>,
    ) -> Expr<'src, (Token<'src>, TypeId)> {
        let token = expr.meta.0;

        let tid = self.request_type(expr.meta.1, substitutions);
        let kind = match expr.expr {
            ExprKind::U8(_)
            | ExprKind::U64(_)
            | ExprKind::Bool(_)
            | ExprKind::String(_)
            | ExprKind::CString(_)
            | ExprKind::Char(_)
            | ExprKind::Bind(_)
            | ExprKind::Projection(_)
            | ExprKind::Pack
            | ExprKind::Unpack
            | ExprKind::Call
            | ExprKind::SizeOf
            | ExprKind::UncheckedCast
            | ExprKind::Read
            | ExprKind::Write => expr.expr,

            ExprKind::Place(place) => {
                if (self.world.functions.contains_key(&place.base)
                    || self.world.interface_fns.contains_key(&place.base))
                    && place.projections.is_empty()
                {
                    ExprKind::Place(Place {
                        base: self.resolve_function_instance(
                            &token,
                            place.base,
                            tid,
                            substitutions,
                        ),
                        projections: vec![],
                    })
                } else {
                    ExprKind::Place(place)
                }
            }
            ExprKind::Ref { place, mutability } => {
                if self.world.functions.contains_key(&place.base) && place.projections.is_empty() {
                    ExprKind::Ref {
                        place: Place {
                            base: self.resolve_function_instance(
                                &token,
                                place.base,
                                tid,
                                substitutions,
                            ),
                            projections: vec![],
                        },
                        mutability,
                    }
                } else {
                    ExprKind::Ref { place, mutability }
                }
            }
            ExprKind::Block(exprs) => ExprKind::Block(
                exprs
                    .into_iter()
                    .map(|e| self.monomorphize_expr(e, substitutions))
                    .collect(),
            ),
            ExprKind::Tuple(exprs) => ExprKind::Tuple(
                exprs
                    .into_iter()
                    .map(|e| self.monomorphize_expr(e, substitutions))
                    .collect(),
            ),
            ExprKind::Type(tid) => ExprKind::Type(self.request_type(tid, substitutions)),
            ExprKind::Add(tid) => ExprKind::Add(self.request_type(tid, substitutions)),
            ExprKind::Sub(tid) => ExprKind::Sub(self.request_type(tid, substitutions)),
            ExprKind::Mul(tid) => ExprKind::Mul(self.request_type(tid, substitutions)),
            ExprKind::Equal(tid) => ExprKind::Equal(self.request_type(tid, substitutions)),
            ExprKind::LessThan(tid) => ExprKind::LessThan(self.request_type(tid, substitutions)),
            ExprKind::Match(match_expr) => {
                let cases = match_expr
                    .cases
                    .into_iter()
                    .map(|case| {
                        let pattern = self.monomorphize_pattern(case.pattern, substitutions);
                        let expr = self.monomorphize_expr(*case.expr, substitutions);
                        MatchCase::new(pattern, expr)
                    })
                    .collect();
                let finally = match_expr
                    .finally
                    .map(|expr| Box::new(self.monomorphize_expr(*expr, substitutions)));

                ExprKind::Match(MatchExpr { cases, finally })
            }
            ExprKind::If(if_expr) => ExprKind::If(IfExpr {
                then: Box::new(self.monomorphize_expr(*if_expr.then, substitutions)),
                otherwise: Box::new(self.monomorphize_expr(*if_expr.otherwise, substitutions)),
            }),
            ExprKind::While { condition, body } => ExprKind::While {
                condition: condition
                    .into_iter()
                    .map(|e| self.monomorphize_expr(e, substitutions))
                    .collect(),
                body: Box::new(self.monomorphize_expr(*body, substitutions)),
            },
        };

        Expr {
            expr: kind,
            meta: (token, tid),
        }
    }

    fn infer_function_args(
        &mut self,
        token: &Token<'src>,
        var: Var,
        instantiated_tid: TypeId,
        substitutions: &HashMap<TypeVar, TypeId>,
    ) -> Vec<TypeId> {
        let scheme = self.env.get_var(&var).unwrap().clone();

        let concrete_tid = self.request_type(instantiated_tid, substitutions);

        // Pattern: fn(T) -> T
        // Actual:  fn(u64) -> u64
        //
        // Result: T -> u64
        let inferred = self
            .typestore
            .assign_vars_from_concrete(&mut self.env, &scheme, concrete_tid, token)
            .map_err(|e| match e {
                InferenceErrorKind::TypesIncompatible { expected, found } => {
                    format!(
                        "Types incompatible: {}",
                        self.typestore.fmt_many(&[expected, found])
                    )
                }
                e => todo!("{e:?}"),
            })
            .unwrap();

        scheme
            .vars
            .iter()
            .enumerate()
            .map(|(idx, _)| inferred[idx])
            .collect()
    }

    fn resolve_function_instance(
        &mut self,
        token: &Token<'src>,
        var: Var,
        instantiated_tid: TypeId,
        substitutions: &HashMap<TypeVar, TypeId>,
    ) -> Var {
        // Recover the concrete type arguments used at this reference.
        let instance = if let Some(cid) = self.world.interface_fns.get(&var) {
            let mut instance = None;
            for typed_impl in self.world.interface_impls.get(cid).unwrap() {
                let mapped_var = typed_impl.fn_map.get(&var).unwrap();

                let qualified = self
                    .typestore
                    .freshen(self.env.get_var(mapped_var).unwrap(), token);
                let tid = qualified.tid;
                let fresh_tid = self.typestore.freshen_all(instantiated_tid, token);

                if self
                    .typestore
                    .unify_ty_ty(&self.env, tid, fresh_tid, token)
                    .is_ok()
                {
                    let (_, tid) = self.typestore.normalize_ty(&self.env, tid, token).unwrap();
                    // TODO: Check predicates.
                    let args = self.infer_function_args(token, *mapped_var, tid, substitutions);

                    instance = Some(FunctionInstance {
                        var: *mapped_var,
                        args,
                    });
                    break;
                }
            }
            match instance {
                Some(instance) => instance,
                None => todo!(),
            }
        } else {
            let args = self.infer_function_args(token, var, instantiated_tid, substitutions);
            FunctionInstance { var, args }
        };

        self.request_function(instance)
    }
}

#[derive(Default)]
pub struct TypedWorld<'src> {
    functions: HashMap<Var, TypedFunction<'src>>,
    interface_impls: HashMap<TypeClassId, Vec<TypedInterfaceImpl>>,
    interface_fns: HashMap<Var, TypeClassId>,
}

impl<'src> TypedWorld<'src> {
    fn add_func(&mut self, var: Var, func: TypedFunction<'src>) {
        assert!(self.functions.insert(var, func).is_none())
    }

    fn add_impl(&mut self, cid: TypeClassId, typed_impl: TypedInterfaceImpl) {
        if self.interface_impls.contains_key(&cid) {
            self.interface_impls.get_mut(&cid).unwrap().push(typed_impl)
        } else {
            self.interface_impls.insert(cid, vec![typed_impl]);
        }
    }

    pub fn monomorphize(
        mut self,
        typestore: &mut TypeStore<Token<'src>>,
        env: &mut Environment,
        entry: Var,
    ) -> (Vec<TypeId>, Vec<(Var, TypedFunction<'src>)>, (Var, TypeId)) {
        let entry_tid = env.get_var(&entry).unwrap().tid;
        let mut ctx = MonoContext::new(&self, env, typestore);

        // Register the entry point with no generic arguments.
        let entry = ctx.request_function(FunctionInstance {
            var: entry,
            args: vec![],
        });

        // Process each requested specialization.
        while let Some(instance) = ctx.pending.pop() {
            ctx.monomorphize_function(instance);
        }

        let mut types = Vec::from_iter(ctx.types.into_iter());
        types.sort();

        (types, ctx.functions, (entry, entry_tid))
    }
}

#[cfg(test)]
mod tests {
    use std::assert_eq;

    use crate::typecheck::{
        constraint::ConstraintOrigin,
        environment::{Coherence, Environment, Interface, InterfaceImpl, Scheme, TypePredicate},
        expr::{Expr, ExprKind, MatchCase, MatchExpr, MatchPattern, Place, PlaceProjection, Var},
        func::Function,
        typ::{
            typ::{EnumDef, FunctionType, Mutability, PointerType, Projection, StructDef, Type},
            type_id::TypeId,
        },
        typestore::{
            inference::{InferenceError, InferenceErrorKind},
            MemberAccess, MemberLookup, TypeStore,
        },
    };

    use super::typ::type_id::StructId;

    const META: () = ();
    const ORIGIN: ConstraintOrigin = ConstraintOrigin::ConflictingImpls;
    const LOOKUP: () = ();

    impl MemberLookup for () {
        fn get_member_idx(&self, sid: StructId, member: &str) -> Option<usize> {
            None
        }
    }

    #[test]
    fn dup() {
        // Create the TypeStore and the Environment
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // fn dup<T>(T: t) -> [T T] { t t }
        // Create a new typevar for the identifier `t`
        let t = Var(0);
        // Create a typevar for the type `T`
        let typevar = typestore.fresh_type_var();
        // Insert the Type `T` into the typestore and get a tid.
        let tid = typestore.intern(Type::TypeVar(typevar));

        let dup = Function::new(
            [tid],
            [tid, tid],
            [typevar],
            [],
            Expr::block(
                [Expr::bind(t, ()), Expr::ident(t, ()), Expr::ident(t, ())],
                (),
            ),
        );

        let f = dup.type_check(&mut typestore, &env, &LOOKUP);
        assert!(f.is_ok(), "{f:?}");
    }

    #[test]
    fn bad_dup() {
        // fn dup<T>(T: t) -> [T T] { t 1 }
        // Create the TypeStore and the Environment
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Create a new typevar for the identifier `t`
        let t = Var(0);
        // Create a typevar for the type `T`
        let typevar = typestore.fresh_type_var();
        // Insert the Type `T` into the typestore and get a tid.
        let tid = typestore.intern(Type::TypeVar(typevar));

        let bad_dup = Function::new(
            [tid],
            [tid, tid],
            [typevar],
            [],
            Expr::block([Expr::bind(t, ()), Expr::ident(t, ()), Expr::u8(0, ())], ()),
        );

        let rigid = typestore.intern(Type::RigidTypeVar(typevar));
        let u8_tid = typestore.intern(Type::u8());
        let f = bad_dup.type_check(&mut typestore, &env, &LOOKUP);
        assert_eq!(
            f,
            Err(InferenceError::type_param_constrained(
                typevar,
                typestore.intern(Type::u8()),
                &ConstraintOrigin::OutputSubtyping {
                    expected: vec![tid, tid],
                    found: vec![rigid, u8_tid]
                },
                &META
            ))
        );
    }

    #[test]
    fn call_dup_with_concrete() {
        // fn dup<T>(T: t) -> [T T]
        // fn test() -> [u8 u8] {
        //     1 dup
        // }
        // Create the TypeStore and the Environment
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        // Create a new typevar for the identifier `dup`
        let dup = Var(0);
        // Create a typevar for the type `T`
        let dup_typevar = typestore.fresh_type_var();
        // Insert the Type `T` into the typestore and get a tid.
        let dup_typevar_tid = typestore.intern(Type::TypeVar(dup_typevar));
        let dup_fn_ty = Type::Function(FunctionType {
            input: vec![dup_typevar_tid],
            output: vec![dup_typevar_tid, dup_typevar_tid].into(),
        });

        let dup_tid = typestore.intern(dup_fn_ty);

        // Map the identifier `t` to the TypeId for `T` in the environment
        let dup_scheme = Scheme::new([dup_typevar], dup_tid, []);
        env.bind_scheme(dup, dup_scheme);

        let u8_tid = typestore.intern(Type::u8());

        let test = Function::new(
            [],
            [u8_tid, u8_tid],
            [],
            [],
            Expr::block([Expr::u8(0, ()), Expr::ident(dup, ())], ()),
        );

        let f = test.type_check(&mut typestore, &env, &LOOKUP);
        assert!(f.is_ok(), "{f:?}");
    }

    #[test]
    fn gt() {
        // fn test() -> [bool] {
        //     1 2 >
        // }

        // Create the TypeStore and the Environment
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let u8_tid = typestore.intern(Type::u8());
        let bool_tid = typestore.intern(Type::bool());

        // Create a new typevar for the "function" `>`
        let gt = Var(0);
        let ty = Type::Function(FunctionType {
            input: vec![u8_tid, u8_tid],
            output: vec![bool_tid].into(),
        });
        let tid = typestore.intern(ty);

        // Map the identifier `t` to the TypeId for `T` in the environment
        let gt_scheme = Scheme::new([], tid, []);
        env.bind_scheme(gt, gt_scheme);

        // Manually create the body of the expression.
        let expr = Expr::block([Expr::u8(0, ()), Expr::u8(0, ()), Expr::ident(gt, ())], ());

        let stack = vec![];
        let target = vec![bool_tid].into();

        assert!(typestore
            .type_check(expr, &stack, &target, &mut env, &LOOKUP)
            .is_ok())
    }

    #[test]
    fn not_enough_args() {
        // foo foo(u8);
        // fn test() foo

        // Create the TypeStore and the Environment
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let u8_tid = typestore.intern(Type::u8());

        // Create a new typevar for the "function" `>`
        let foo = Var(0);
        let ty = Type::Function(FunctionType {
            input: vec![u8_tid],
            output: vec![].into(),
        });
        let tid = typestore.intern(ty);

        // Map the identifier `t` to the TypeId for `T` in the environment
        let foo_scheme = Scheme::new([], tid, []);
        env.bind_scheme(foo, foo_scheme);

        // Manually create the body of the expression.
        let expr = Expr::ident(foo, ());

        let stack = vec![];
        let target = vec![].into();

        assert_eq!(
            typestore.type_check(expr, &stack, &target, &mut env, &LOOKUP),
            Err(InferenceError::not_enough_args(
                vec![u8_tid],
                vec![],
                &ConstraintOrigin::FunctionCall {
                    input: vec![],
                    expected: vec![u8_tid]
                },
                &META
            ))
        )
    }

    #[test]
    fn swap() {
        // fn swap<A, B>(A:a B:b) -> [B A] { b a }
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let a = Var(0);
        let a_typevar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_typevar));

        let b = Var(1);
        let b_typevar = typestore.fresh_type_var();
        let b_tid = typestore.intern(Type::TypeVar(b_typevar));

        let swap = Function::new(
            [a_tid, b_tid],
            [b_tid, a_tid],
            [a_typevar, b_typevar],
            [],
            Expr::block(
                [
                    Expr::bind(b, ()),
                    Expr::bind(a, ()),
                    Expr::ident(b, ()),
                    Expr::ident(a, ()),
                ],
                (),
            ),
        );

        assert!(swap.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn bad_swap() {
        // fn bad_swap<A, B>(A:a B:b) -> [B A] { a b }
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let a = Var(0);
        let a_typevar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_typevar));

        let b = Var(1);
        let b_typevar = typestore.fresh_type_var();
        let b_tid = typestore.intern(Type::TypeVar(b_typevar));

        let swap = Function::new(
            [a_tid, b_tid],
            [b_tid, a_tid],
            [a_typevar, b_typevar],
            [],
            Expr::block(
                [
                    Expr::bind(b, ()),
                    Expr::bind(a, ()),
                    Expr::ident(a, ()),
                    Expr::ident(b, ()),
                ],
                (),
            ),
        );

        match swap.type_check(&mut typestore, &env, &LOOKUP) {
            Err(InferenceError {
                kind: InferenceErrorKind::TypeParameterConstrained { var, .. },
                ..
            }) if var == a_typevar => assert!(true),
            e => todo!("{e:?}"),
        }
    }

    #[test]
    fn unbound_type() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        // Setup for the `drop` fn in the environment
        let t_typevar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_typevar));

        let drop = Var(1);
        let drop_ty = Type::Function(FunctionType {
            input: vec![t_tid],
            output: vec![].into(),
        });

        let drop_tid = typestore.intern(drop_ty);
        let drop_scheme = Scheme::new([t_typevar], drop_tid, []);
        env.bind_scheme(drop, drop_scheme);

        // fn foo<T>() -> [T]
        let foo_param_typevar = typestore.fresh_type_var();
        let foo_param_tid = typestore.intern(Type::TypeVar(foo_param_typevar));
        let foo = Var(2);
        let foo_ty = Type::Function(FunctionType {
            input: vec![],
            output: vec![foo_param_tid].into(),
        });
        let foo_tid = typestore.intern(foo_ty);
        let foo_scheme = Scheme::new([foo_param_typevar], foo_tid, []);
        env.bind_scheme(foo, foo_scheme);

        let test = Function::new(
            [],
            [],
            [],
            [],
            Expr::block([Expr::ident(foo, ()), Expr::ident(drop, ())], ()),
        );

        assert!(matches!(
            test.type_check(&mut typestore, &env, &LOOKUP),
            Err(InferenceError {
                kind: InferenceErrorKind::UnboundVar(_),
                ..
            })
        ));
    }

    #[test]
    fn different_binds() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        // Setup for the `dup` fn in the environment
        let t_typevar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_typevar));

        let dup = Var(1);
        let dup_ty = Type::Function(FunctionType {
            input: vec![t_tid],
            output: vec![t_tid, t_tid].into(),
        });

        let dup_tid = typestore.intern(dup_ty);
        let dup_scheme = Scheme::new([t_typevar], dup_tid, []);
        env.bind_scheme(dup, dup_scheme);

        let u8_tid = typestore.intern(Type::u8());
        let bool_tid = typestore.intern(Type::bool());
        // fn test() -> [u8 u8 bool bool]
        let test = Function::new(
            [],
            [u8_tid, u8_tid, bool_tid, bool_tid],
            [],
            [],
            Expr::block(
                [
                    Expr::u8(0, ()),
                    Expr::ident(dup, ()),
                    Expr::bool(true, ()),
                    Expr::ident(dup, ()),
                ],
                (),
            ),
        );
        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn simple_mismatch() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let u8_tid = typestore.intern(Type::u8());
        let bool_tid = typestore.intern(Type::bool());
        let test = Function::new([], [u8_tid], [], [], Expr::bool(true, ()));

        assert_eq!(
            test.type_check(&mut typestore, &env, &LOOKUP),
            Err(InferenceError::types_incompatible(
                u8_tid,
                bool_tid,
                &ConstraintOrigin::OutputSubtyping {
                    expected: vec![u8_tid],
                    found: vec![bool_tid]
                },
                &META
            ))
        );
    }

    #[test]
    fn simple_ref() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let x = Place::ident(Var(0));

        let u8_tid = typestore.intern(Type::u8());
        let u8_const_ptr = Type::Pointer(PointerType {
            tid: u8_tid,
            mutability: Mutability::Const,
        });

        let u8_const_ptr_tid = typestore.intern(u8_const_ptr);

        let test = Function::new(
            [u8_tid],
            [u8_const_ptr_tid],
            [],
            [],
            Expr::block(
                [
                    Expr::bind(x.base, ()),
                    Expr::take_ref(x, Mutability::Const, ()),
                ],
                (),
            ),
        );

        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn fn_pointer_call() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let dup = Place::ident(Var(0));
        let dup_tyvar = typestore.fresh_type_var();
        let dup_t_tid = typestore.intern(Type::TypeVar(dup_tyvar));
        let dup_tid = typestore.intern(Type::Function(FunctionType {
            input: vec![dup_t_tid],
            output: vec![dup_t_tid, dup_t_tid].into(),
        }));

        let dup_scheme = Scheme::new([dup_tyvar], dup_tid, vec![]);
        env.bind_scheme(dup.base, dup_scheme);

        let test = Function::new(
            [],
            [typestore.intern(Type::u8()); 2],
            [],
            [],
            Expr::block(
                [
                    Expr::u8(0, ()),
                    Expr::take_ref(dup, Mutability::Const, ()),
                    Expr::call(()),
                ],
                (),
            ),
        );
        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn mutable_fn_pointer() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let f = Place::ident(Var(0));
        let func_tid = typestore.function(FunctionType {
            input: vec![],
            output: vec![].into(),
        });
        let func_scheme = Scheme::new([], func_tid, []);
        env.bind_scheme(f.base, func_scheme);

        let test = Function::new(
            [],
            [func_tid],
            [],
            [],
            Expr::take_ref(f.clone(), Mutability::Mutable, ()),
        );

        assert_eq!(
            test.type_check(&mut typestore, &env, &LOOKUP),
            Err(InferenceError::mutable_ref_to_fn(
                f.base,
                &ConstraintOrigin::TakeRef {
                    var: Var(0),
                    mutability: Mutability::Mutable,
                },
                &META
            ))
        );
    }

    #[test]
    fn fn_pointer_bad_args() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let u8_tid = typestore.intern(Type::u8());
        let bool_tid = typestore.intern(Type::bool());
        let f = Place::ident(Var(0));
        let func_tid = typestore.function(FunctionType {
            input: vec![u8_tid],
            output: vec![].into(),
        });

        let func_scheme = Scheme::new([], func_tid, []);
        env.bind_scheme(f.base, func_scheme);

        let test_not_enough_args = Function::new(
            [],
            [],
            [],
            [],
            Expr::block(
                [
                    Expr::take_ref(f.clone(), Mutability::Const, ()),
                    Expr::call(()),
                ],
                (),
            ),
        );

        assert_eq!(
            test_not_enough_args.type_check(&mut typestore, &env, &LOOKUP),
            Err(InferenceError::not_enough_args(
                vec![u8_tid],
                vec![],
                &ConstraintOrigin::FunctionCall {
                    input: vec![],
                    expected: vec![u8_tid]
                },
                &META
            ))
        );

        let test_bad_args = Function::new(
            [],
            [],
            [],
            [],
            Expr::block(
                [
                    Expr::bool(true, ()),
                    Expr::take_ref(f.clone(), Mutability::Const, ()),
                    Expr::call(()),
                ],
                (),
            ),
        );

        assert_eq!(
            test_bad_args.type_check(&mut typestore, &env, &LOOKUP),
            Err(InferenceError::types_incompatible(
                u8_tid,
                bool_tid,
                &ConstraintOrigin::FunctionCall {
                    input: vec![bool_tid],
                    expected: vec![u8_tid]
                },
                &META
            ))
        );
    }

    #[test]
    fn generic_pointer() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let t_typevar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_typevar));
        let t_ptr_tid = typestore.pointer(t_tid, Mutability::Mutable);

        let foo = Var(0);
        let foo_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![t_ptr_tid].into(),
        });

        let foo_scheme = Scheme::new([t_typevar], foo_tid, []);
        env.bind_scheme(foo, foo_scheme);

        let test = Function::new([t_tid], [t_ptr_tid], [t_typevar], [], Expr::ident(foo, ()));

        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn pointer_subtyping() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let take_const_ptr = Var(0);

        let u8_tid = typestore.intern(Type::u8());
        let u8_const_ptr_tid = typestore.pointer(u8_tid, Mutability::Const);
        let u8_mut_ptr_tid = typestore.pointer(u8_tid, Mutability::Mutable);
        let take_const_ptr_tid = typestore.function(FunctionType {
            input: vec![u8_const_ptr_tid],
            output: vec![].into(),
        });

        let take_const_ptr_scheme = Scheme::new([], take_const_ptr_tid, []);
        env.bind_scheme(take_const_ptr, take_const_ptr_scheme);

        let test1 = Function::new(
            [u8_mut_ptr_tid],
            [],
            [],
            [],
            Expr::ident(take_const_ptr, ()),
        );

        assert!(test1.type_check(&mut typestore, &env, &LOOKUP).is_ok());

        let identity = Var(1);
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let identity_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![t_tid].into(),
        });

        let identity_scheme = Scheme::new([t_tyvar], identity_tid, []);
        env.bind_scheme(identity, identity_scheme);

        let test2 = Function::new(
            [u8_mut_ptr_tid],
            [u8_const_ptr_tid],
            [],
            [],
            Expr::block([Expr::ident(identity, ()), Expr::ident(identity, ())], ()),
        );
        assert!(test2.type_check(&mut typestore, &env, &LOOKUP).is_ok());

        let test3 = Function::new(
            [u8_const_ptr_tid],
            [u8_mut_ptr_tid],
            [],
            [],
            Expr::block([], ()),
        );
        assert_eq!(
            test3.type_check(&mut typestore, &env, &LOOKUP),
            Err(InferenceError::types_incompatible(
                u8_mut_ptr_tid,
                u8_const_ptr_tid,
                &ConstraintOrigin::OutputSubtyping {
                    expected: vec![u8_mut_ptr_tid],
                    found: vec![u8_const_ptr_tid]
                },
                &META
            ))
        );
    }

    #[test]
    fn first_class_types() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let t_typevar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_typevar));
        let t_typ_tid = typestore.metatype(&env, t_tid, None, &META).unwrap();
        let u8_tid = typestore.intern(Type::u8());
        let u8_t_tid = typestore.metatype(&env, u8_tid, None, &META).unwrap();

        let sizeof = Var(0);
        let sizeof_tid = typestore.function(FunctionType {
            input: vec![t_typ_tid],
            output: vec![u8_tid].into(),
        });
        let sizeof_scheme = Scheme::new([t_typevar], sizeof_tid, []);
        env.bind_scheme(sizeof, sizeof_scheme);

        let test = Function::new(
            [],
            [u8_tid],
            [],
            [],
            Expr::block([Expr::typ(u8_t_tid, ()), Expr::ident(sizeof, ())], ()),
        );
        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn type_of() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let t_ty_tid = typestore.metatype(&env, t_tid, None, &META).unwrap();

        let type_of = Var(0);
        let type_of_impl = Function::new(
            [t_tid],
            [t_ty_tid],
            [t_tyvar],
            [],
            Expr::block([Expr::bind(Var(1), ()), Expr::typ(t_ty_tid, ())], ()),
        );
        let type_of_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![t_ty_tid].into(),
        });

        let type_of_scheme = Scheme::new([t_tyvar], type_of_tid, []);

        env.bind_scheme(type_of, type_of_scheme);
        assert!(type_of_impl
            .type_check(&mut typestore, &env, &LOOKUP)
            .is_ok());

        let u8_tid = typestore.intern(Type::u8());
        let u8_t_tid = typestore.metatype(&env, u8_tid, None, &META).unwrap();

        let call_type_of = Function::new(
            [],
            [u8_t_tid],
            [],
            [],
            Expr::block([Expr::u8(0, ()), Expr::ident(type_of, ())], ()),
        );

        assert!(call_type_of
            .type_check(&mut typestore, &env, &LOOKUP)
            .is_ok());
    }

    #[test]
    fn impl_not_found() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let t_tyvar = typestore.fresh_type_var();
        let foo = Interface {
            vars: vec![t_tyvar],
            coherence: Coherence::Unique,
            associated_types_count: 0,
        };

        let foo_cid = env.add_typeclass(foo);

        let bar = Var(0);
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let bar_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![].into(),
        });
        let bar_scheme = Scheme::new(
            [t_tyvar],
            bar_tid,
            [TypePredicate {
                cid: foo_cid,
                args: vec![t_tid],
            }],
        );
        env.bind_scheme(bar, bar_scheme);

        let u8_tid = typestore.intern(Type::u8());

        let test = Function::new([u8_tid], [], [], [], Expr::ident(Var(0), ()));
        assert_eq!(
            test.type_check(&mut typestore, &env, &LOOKUP),
            Err(InferenceError::predicate_not_met(
                foo_cid,
                vec![u8_tid],
                &ConstraintOrigin::FunctionCall {
                    input: vec![u8_tid],
                    expected: vec![TypeId(5)]
                },
                &META
            ))
        );
    }

    #[test]
    fn impl_found_basic() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let t_tyvar = typestore.fresh_type_var();
        let foo = Interface {
            vars: vec![t_tyvar],
            coherence: Coherence::Unique,
            associated_types_count: 0,
        };

        let foo_cid = env.add_typeclass(foo);

        let u8_tid = typestore.intern(Type::u8());
        assert!(env
            .add_impl(
                &mut typestore,
                foo_cid,
                InterfaceImpl {
                    cid: foo_cid,
                    vars: vec![],
                    args: vec![u8_tid],
                    associated_types: vec![],
                    predicates: vec![],
                    coherence: Coherence::Unique,
                },
                &META
            )
            .is_ok());

        let bar = Var(0);
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let bar_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![].into(),
        });
        let bar_scheme = Scheme::new(
            [t_tyvar],
            bar_tid,
            [TypePredicate {
                cid: foo_cid,
                args: vec![t_tid],
            }],
        );
        env.bind_scheme(bar, bar_scheme);

        let test = Function::new([u8_tid], [], [], [], Expr::ident(Var(0), ()));
        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn generic_impl() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let from_tyvar = typestore.fresh_type_var();
        let into_tyvar = typestore.fresh_type_var();

        let cast_iface = Interface {
            vars: vec![from_tyvar, into_tyvar],
            coherence: Coherence::Overlapping,
            associated_types_count: 0,
        };
        let cast_cid = env.add_typeclass(cast_iface);

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));
        let a_ptr_tid = typestore.pointer(a_tid, Mutability::Const);
        let b_ptr_tid = typestore.pointer(b_tid, Mutability::Const);

        // impl<A B> Cast<&A &B> { ... }
        assert!(env
            .add_impl(
                &mut typestore,
                cast_cid,
                InterfaceImpl {
                    cid: cast_cid,
                    vars: vec![a_tyvar, b_tyvar],
                    args: vec![a_ptr_tid, b_ptr_tid],
                    associated_types: vec![],
                    predicates: vec![],
                    coherence: Coherence::Overlapping,
                },
                &META
            )
            .is_ok());

        let a_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));

        // impl<A> Castable<A A> { ... }
        assert!(env
            .add_impl(
                &mut typestore,
                cast_cid,
                InterfaceImpl {
                    cid: cast_cid,
                    vars: vec![a_tyvar],
                    args: vec![a_tid, a_tid],
                    associated_types: vec![],
                    predicates: vec![],
                    coherence: Coherence::Overlapping,
                },
                &META
            )
            .is_ok());

        let cast = Var(0);
        let from_tyvar = typestore.fresh_type_var();
        let into_tyvar = typestore.fresh_type_var();
        let from_tid = typestore.intern(Type::TypeVar(from_tyvar));
        let into_tid = typestore.intern(Type::TypeVar(into_tyvar));
        let cast_tid = typestore.function(FunctionType {
            input: vec![from_tid],
            output: vec![into_tid].into(),
        });

        // fn cast<From Into>(From) -> [Into] requires: Castable<From Into>
        let cast_scheme = Scheme::new(
            [from_tyvar, into_tyvar],
            cast_tid,
            [TypePredicate {
                cid: cast_cid,
                args: vec![from_tid, into_tid],
            }],
        );
        env.bind_scheme(cast, cast_scheme);

        let u8_tid = typestore.intern(Type::u8());
        let u8_ptr_tid = typestore.pointer(u8_tid, Mutability::Const);
        let bool_tid = typestore.intern(Type::bool());
        let bool_ptr_tid = typestore.pointer(bool_tid, Mutability::Const);

        let test = Function::new([u8_ptr_tid], [bool_ptr_tid], [], [], Expr::ident(cast, ()));
        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());

        let test = Function::new([u8_tid], [bool_ptr_tid], [], [], Expr::ident(cast, ()));
        assert_eq!(
            test.type_check(&mut typestore, &env, &LOOKUP),
            Err(InferenceError::predicate_not_met(
                cast_cid,
                vec![u8_tid, bool_ptr_tid],
                &ConstraintOrigin::FunctionCall {
                    input: vec![u8_tid],
                    expected: vec![TypeId(24)]
                },
                &META
            ))
        );

        let test = Function::new([u8_ptr_tid], [bool_tid], [], [], Expr::ident(cast, ()));
        assert_eq!(
            test.type_check(&mut typestore, &env, &LOOKUP),
            Err(InferenceError::predicate_not_met(
                cast_cid,
                vec![u8_ptr_tid, bool_tid],
                &ConstraintOrigin::FunctionCall {
                    input: vec![u8_ptr_tid],
                    expected: vec![TypeId(33)]
                },
                &META
            ))
        );
    }

    #[test]
    fn generic_impl_with_predicate() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let print_t_tyvar = typestore.fresh_type_var();
        let print_iface = Interface {
            vars: vec![print_t_tyvar],
            coherence: Coherence::Unique,
            associated_types_count: 0,
        };
        let print_cid = env.add_typeclass(print_iface);

        let u8_tid = typestore.intern(Type::u8());
        let u8_ptr_tid = typestore.pointer(u8_tid, Mutability::Const);
        let bool_tid = typestore.intern(Type::bool());
        let bool_ptr_tid = typestore.pointer(bool_tid, Mutability::Const);

        let u8_print_impl = InterfaceImpl {
            cid: print_cid,
            vars: vec![],
            args: vec![u8_tid],
            associated_types: vec![],
            predicates: vec![],
            coherence: Coherence::Unique,
        };
        assert!(env
            .add_impl(&mut typestore, print_cid, u8_print_impl, &META)
            .is_ok());

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let t_ptr_tid = typestore.pointer(t_tid, Mutability::Const);
        let ptr_impl = InterfaceImpl {
            cid: print_cid,
            vars: vec![t_tyvar],
            args: vec![t_ptr_tid],
            associated_types: vec![],
            predicates: vec![TypePredicate {
                cid: print_cid,
                args: vec![t_tid],
            }],
            coherence: Coherence::Unique,
        };
        assert!(env
            .add_impl(&mut typestore, print_cid, ptr_impl, &META)
            .is_ok());

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let print_fn_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![].into(),
        });
        let print_scheme = Scheme::new(
            [t_tyvar],
            print_fn_tid,
            [TypePredicate {
                cid: print_cid,
                args: vec![t_tid],
            }],
        );
        let print = Var(0);
        env.bind_scheme(print, print_scheme);

        let u8_print = Function::new([u8_tid], [], [], [], Expr::ident(print, ()));
        assert!(u8_print.type_check(&mut typestore, &env, &LOOKUP).is_ok());

        let bool_print = Function::new([bool_tid], [], [], [], Expr::ident(print, ()));
        assert_eq!(
            bool_print.type_check(&mut typestore, &env, &LOOKUP),
            Err(InferenceError::predicate_not_met(
                print_cid,
                vec![bool_tid],
                &ConstraintOrigin::FunctionCall {
                    input: vec![bool_tid],
                    expected: vec![TypeId(18)]
                },
                &META
            ))
        );

        let u8_ptr_print = Function::new([u8_ptr_tid], [], [], [], Expr::ident(print, ()));
        assert!(u8_ptr_print
            .type_check(&mut typestore, &env, &LOOKUP)
            .is_ok());

        let bool_ptr_print = Function::new([bool_ptr_tid], [], [], [], Expr::ident(print, ()));
        assert_eq!(
            bool_ptr_print.type_check(&mut typestore, &env, &LOOKUP),
            Err(InferenceError::predicate_not_met(
                print_cid,
                vec![bool_ptr_tid],
                &ConstraintOrigin::FunctionCall {
                    input: vec![TypeId(4)],
                    expected: vec![TypeId(32)]
                },
                &META
            ))
        );

        let u8_ptr_ptr_tid = typestore.pointer(u8_ptr_tid, Mutability::Const);

        let u8_ptr_ptr_print = Function::new([u8_ptr_ptr_tid], [], [], [], Expr::ident(print, ()));

        assert!(u8_ptr_ptr_print
            .type_check(&mut typestore, &env, &LOOKUP)
            .is_ok());

        let bool_ptr_ptr_tid = typestore.pointer(bool_ptr_tid, Mutability::Const);

        let bool_ptr_ptr_print =
            Function::new([bool_ptr_ptr_tid], [], [], [], Expr::ident(print, ()));

        assert_eq!(
            bool_ptr_ptr_print.type_check(&mut typestore, &env, &LOOKUP),
            Err(InferenceError::predicate_not_met(
                print_cid,
                vec![bool_ptr_ptr_tid],
                &ConstraintOrigin::FunctionCall {
                    input: vec![TypeId(51)],
                    expected: vec![TypeId(52)]
                },
                &META
            ))
        );
    }

    #[test]
    fn call_interface_fn() {
        // interface Print<T> {
        //     fn print(T)
        //     fn println(T) { print "\n" print }
        //     ~~~~~~~~~~
        //     fn println<T>(T) requires: Print<T> { print "\n" print }
        // }
        //
        // impl Print<u8> {
        //     print(u8) { ... }
        // }
        //
        // fn main() {
        //     123 print
        //     bool print //< Error
        // }

        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let print_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![].into(),
        });

        let print_iface = Interface {
            vars: vec![t_tyvar],
            coherence: Coherence::Unique,
            associated_types_count: 0,
        };

        let print_cid = env.add_typeclass(print_iface);
        let print_scheme = Scheme::new(
            [t_tyvar],
            print_tid,
            [TypePredicate {
                cid: print_cid,
                args: vec![t_tid],
            }],
        );
        let print = Var(0);

        env.bind_scheme(print, print_scheme);

        let u8_tid = typestore.intern(Type::u8());
        let bool_tid = typestore.intern(Type::bool());

        let print_impl_u8 = InterfaceImpl {
            cid: print_cid,
            vars: vec![],
            args: vec![u8_tid],
            associated_types: vec![],
            predicates: vec![],
            coherence: Coherence::Unique,
        };

        let print_impl_bool = InterfaceImpl {
            cid: print_cid,
            vars: vec![],
            args: vec![bool_tid],
            associated_types: vec![],
            predicates: vec![],
            coherence: Coherence::Unique,
        };

        assert!(env
            .add_impl(&mut typestore, print_cid, print_impl_u8, &META)
            .is_ok());

        let main = Function::new([u8_tid], [], [], [], Expr::ident(print, ()));
        assert!(main.type_check(&mut typestore, &env, &LOOKUP).is_ok());
        let main = Function::new([bool_tid], [], [], [], Expr::ident(print, ()));
        assert_eq!(
            main.type_check(&mut typestore, &env, &LOOKUP),
            Err(InferenceError::predicate_not_met(
                print_cid,
                vec![bool_tid],
                &ConstraintOrigin::FunctionCall {
                    input: vec![TypeId(2)],
                    expected: vec![TypeId(10)]
                },
                &META
            ))
        );

        assert!(env
            .add_impl(&mut typestore, print_cid, print_impl_bool, &META)
            .is_ok());
        let main = Function::new(
            [u8_tid, bool_tid],
            [],
            [],
            [],
            Expr::block([Expr::ident(print, ()), Expr::ident(print, ())], ()),
        );
        assert!(main.type_check(&mut typestore, &env, &LOOKUP).is_ok());

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));

        // use u8 as a stand-in for `Str`
        let println = Function::new(
            [t_tid],
            [],
            [t_tyvar],
            [TypePredicate {
                cid: print_cid,
                args: vec![t_tid],
            }],
            Expr::block(
                [
                    Expr::ident(print, ()),
                    Expr::u8(0, ()),
                    Expr::ident(print, ()),
                ],
                (),
            ),
        );
        assert!(println.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn function_with_predicate() {
        // interface Print<T> {
        //     fn print(T)
        //     fn println(T) { print "\n" print }
        //     ~~~~~~~~~~
        // }
        //
        // impl Print<u8> {
        //     print(u8) { ... }
        // }
        //
        // fn println<T>(T) requires: Print<T> { print "\n" print }

        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let print_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![].into(),
        });

        let print_iface = Interface {
            vars: vec![t_tyvar],
            coherence: Coherence::Unique,
            associated_types_count: 0,
        };

        let print_cid = env.add_typeclass(print_iface);
        let print_scheme = Scheme::new(
            [t_tyvar],
            print_tid,
            [TypePredicate {
                cid: print_cid,
                args: vec![t_tid],
            }],
        );
        let print = Var(0);

        env.bind_scheme(print, print_scheme);

        let u8_tid = typestore.intern(Type::u8());

        let print_impl_u8 = InterfaceImpl {
            cid: print_cid,
            vars: vec![],
            args: vec![u8_tid],
            associated_types: vec![],
            predicates: vec![],
            coherence: Coherence::Unique,
        };
        assert!(env
            .add_impl(&mut typestore, print_cid, print_impl_u8, &META)
            .is_ok());

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));

        // use u8 as a stand-in for `Str`
        let println = Function::new(
            [t_tid],
            [],
            [t_tyvar],
            [TypePredicate {
                cid: print_cid,
                args: vec![t_tid],
            }],
            Expr::block(
                [
                    Expr::ident(print, ()),
                    Expr::u8(0, ()),
                    Expr::ident(print, ()),
                ],
                (),
            ),
        );
        assert!(println.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn unchecked_cast() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let from_tyvar = typestore.fresh_type_var();
        let into_tyvar = typestore.fresh_type_var();

        let cast_iface = Interface {
            vars: vec![from_tyvar, into_tyvar],
            coherence: Coherence::Overlapping,
            associated_types_count: 0,
        };

        let cast_cid = env.add_typeclass(cast_iface);

        let from_tyvar = typestore.fresh_type_var();
        let from_tid = typestore.intern(Type::TypeVar(from_tyvar));
        let into_tyvar = typestore.fresh_type_var();
        let into_tid = typestore.intern(Type::TypeVar(into_tyvar));
        let into_ty_tid = typestore.metatype(&env, into_tid, None, &META).unwrap();

        let cast = Function::new(
            [from_tid, into_ty_tid],
            [into_tid],
            [from_tyvar, into_tyvar],
            [TypePredicate {
                cid: cast_cid,
                args: vec![from_tid, into_tid],
            }],
            Expr::unchecked_cast(()),
        );
        assert!(cast.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn create_struct() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // struct Pair<T> { T: left T: right}
        // fn example() -> [Pair<bool>] { true false #Pair pack }

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let pair_def =
            StructDef::new(&typestore, vec![t_tyvar], vec![t_tid, t_tid], &META).unwrap();
        let (pair_tid, _) = typestore.add_struct(pair_def);

        let bool_tid = typestore.intern(Type::bool());
        let pair_bool_tid = typestore
            .type_instance(&env, pair_tid, Some(vec![bool_tid]), &META)
            .unwrap();

        let example = Function::new(
            [],
            [pair_bool_tid],
            [],
            [],
            Expr::block(
                [
                    Expr::bool(true, ()),
                    Expr::bool(true, ()),
                    Expr::typ(typestore.metatype(&env, pair_tid, None, &META).unwrap(), ()),
                    Expr::pack(()),
                ],
                (),
            ),
        );

        assert!(example.type_check(&mut typestore, &env, &LOOKUP).is_ok());

        let bad_pack = Function::new(
            [],
            [pair_bool_tid],
            [],
            [],
            Expr::block(
                [
                    Expr::u8(0, ()),
                    Expr::bool(true, ()),
                    Expr::typ(typestore.metatype(&env, pair_tid, None, &META).unwrap(), ()),
                    Expr::pack(()),
                ],
                (),
            ),
        );

        let u8_tid = typestore.intern(Type::u8());
        assert_eq!(
            bad_pack.type_check(&mut typestore, &env, &LOOKUP),
            Err(InferenceError::types_incompatible(
                bool_tid,
                u8_tid,
                &ConstraintOrigin::FunctionCall {
                    input: vec![u8_tid, bool_tid],
                    expected: vec![TypeId(17), TypeId(17)]
                },
                &META
            ))
        );
    }

    #[test]
    fn create_enum() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // enum Option<T> { T: Some None }
        // fn Option.None<T>() -> [Option::None<T>] { #Option::None pack }

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let option_def = EnumDef::new(
            &typestore,
            vec![t_tyvar],
            vec![
                Some(t_tid), // Some
                None,        // None
            ],
            &META,
        )
        .unwrap();

        let (option_tid, _) = typestore.add_enum(option_def);

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let option_none_tid = typestore.get_enum_variant(option_tid, 1, &META).unwrap();
        let option_none_t_tid = typestore
            .type_instance(&env, option_none_tid, Some(vec![t_tid]), &META)
            .unwrap();
        let option_none_ty_tid = typestore
            .metatype(&env, option_none_tid, None, &META)
            .unwrap();

        let option_none = Function::new(
            [],
            [option_none_t_tid],
            [t_tyvar],
            [],
            Expr::block([Expr::typ(option_none_ty_tid, ()), Expr::pack(())], ()),
        );

        assert!(option_none
            .type_check(&mut typestore, &env, &LOOKUP)
            .is_ok());
    }

    #[test]
    fn variant_subtyping() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // enum Option<T> { T: Some None }
        // fn Option.Some<T>(T) -> [Option<T>] { #Option::Some pack }

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let option_def = EnumDef::new(
            &typestore,
            vec![t_tyvar],
            vec![
                Some(t_tid), // Some
                None,        // None
            ],
            &META,
        )
        .unwrap();

        let (option_tid, _) = typestore.add_enum(option_def);

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let option_t_tid = typestore
            .type_instance(&env, option_tid, Some(vec![t_tid]), &META)
            .unwrap();

        let option_some_tid = typestore.get_enum_variant(option_tid, 0, &META).unwrap();
        let option_some_ty_tid = typestore
            .metatype(&env, option_some_tid, None, &META)
            .unwrap();

        let option_some = Function::new(
            [t_tid],
            [option_t_tid],
            [t_tyvar],
            [],
            Expr::block([Expr::typ(option_some_ty_tid, ()), Expr::pack(())], ()),
        );

        assert!(option_some
            .type_check(&mut typestore, &env, &LOOKUP)
            .is_ok());
    }

    #[test]
    fn bad_variant_subtyping() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        // enum Option<T> { T: Some None }
        // fn Option.Some<T>(T) -> [Option<T>] { #Option::Some pack }

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let option_def = EnumDef::new(
            &typestore,
            vec![t_tyvar],
            vec![
                Some(t_tid), // Some
                None,        // None
            ],
            &META,
        )
        .unwrap();

        let (option_tid, _) = typestore.add_enum(option_def);

        let bool_tid = typestore.intern(Type::bool());
        let option_t_tid = typestore
            .type_instance(&env, option_tid, Some(vec![bool_tid]), &META)
            .unwrap();

        let generate_option = Var(0);
        let generate_option_tid = typestore.function(FunctionType {
            input: vec![],
            output: vec![option_t_tid].into(),
        });
        let generate_option_scheme = Scheme::new([], generate_option_tid, []);
        env.bind_scheme(generate_option, generate_option_scheme);

        let option_some_tid = typestore.get_enum_variant(option_tid, 0, &META).unwrap();
        let option_some_t_tid = typestore
            .type_instance(&env, option_some_tid, Some(vec![bool_tid]), &META)
            .unwrap();

        let take_option_some = Var(1);
        let take_option_some_tid = typestore.function(FunctionType {
            input: vec![option_some_t_tid],
            output: vec![].into(),
        });
        let take_option_some_scheme = Scheme::new([], take_option_some_tid, []);
        env.bind_scheme(take_option_some, take_option_some_scheme);

        let test = Function::new(
            [],
            [],
            [],
            [],
            Expr::block(
                [
                    Expr::ident(generate_option, ()),
                    Expr::ident(take_option_some, ()),
                ],
                (),
            ),
        );
        assert_eq!(
            test.type_check(&mut typestore, &env, &LOOKUP),
            Err(InferenceError::types_incompatible(
                option_some_t_tid,
                option_t_tid,
                &ConstraintOrigin::FunctionCall {
                    input: vec![option_t_tid],
                    expected: vec![option_some_t_tid]
                },
                &META
            ))
        );
    }

    #[test]
    fn member_access() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let u8_tid = typestore.intern(Type::u8());
        let u8_ptr_tid = typestore.pointer(u8_tid, Mutability::Const);

        let str_def = StructDef::new(&typestore, vec![], vec![u8_tid, u8_ptr_tid], &META).unwrap();

        let (str_tid, _) = typestore.add_struct(str_def);

        let place = Place {
            base: Var(0),
            projections: vec![PlaceProjection::Indexed(0, MemberAccess::Direct)],
        };

        let test = Function::new(
            [str_tid],
            [u8_tid],
            [],
            [],
            Expr::block(
                [
                    Expr::bind(place.base, ()),
                    Expr {
                        expr: ExprKind::Place(place),
                        meta: (),
                    },
                ],
                (),
            ),
        );

        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn generic_member_access() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let pair_def =
            StructDef::new(&typestore, vec![t_tyvar], vec![t_tid, t_tid], &META).unwrap();
        let (pair_tid, _) = typestore.add_struct(pair_def);
        let bool_tid = typestore.intern(Type::bool());

        let pair_bool_tid = typestore
            .type_instance(&env, pair_tid, Some(vec![bool_tid]), &META)
            .unwrap();

        let place = Place {
            base: Var(0),
            projections: vec![PlaceProjection::Indexed(0, MemberAccess::Direct)],
        };

        let test = Function::new(
            [pair_bool_tid],
            [bool_tid],
            [],
            [],
            Expr::block(
                [
                    Expr::bind(place.base, ()),
                    Expr {
                        expr: ExprKind::Place(place),
                        meta: META,
                    },
                ],
                (),
            ),
        );
        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn basic_match() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let u8_tid = typestore.intern(Type::u8());
        let option_u8 = EnumDef::new(&typestore, vec![], vec![Some(u8_tid), None], &META).unwrap();
        let (option_u8_tid, _) = typestore.add_enum(option_u8);
        let option_u8_some_tid = typestore.get_enum_variant(option_u8_tid, 0, &META).unwrap();
        let option_u8_none_tid = typestore.get_enum_variant(option_u8_tid, 1, &META).unwrap();

        let unwrap_or_default = Function::new(
            [u8_tid, option_u8_tid],
            [u8_tid],
            [],
            [],
            Expr::match_(
                MatchExpr::new(
                    [
                        MatchCase::new(
                            MatchPattern::Variant(option_u8_some_tid),
                            Expr::block(
                                [Expr::bind([Var(0), Var(1)], ()), Expr::ident(Var(1), ())],
                                (),
                            ),
                        ),
                        MatchCase::new(
                            MatchPattern::Variant(option_u8_none_tid),
                            Expr::block([], ()),
                        ),
                    ],
                    None,
                ),
                (),
            ),
        );

        assert!(unwrap_or_default
            .type_check(&mut typestore, &env, &LOOKUP)
            .is_ok());
    }

    #[test]
    fn match_supertyping_enums() {
        // enum Foo { Bar Baz }
        // enum Option<T> { T: Some None }
        // fn test(Foo) -> [Option<u8>] match {
        //     Foo::Bar { 0 #Option::Some pack }
        //     Foo::Baz {   #Option::None pack }
        // }

        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let foo_def = EnumDef::new(&typestore, vec![], vec![None, None], &META).unwrap();
        let (foo_tid, _) = typestore.add_enum(foo_def);
        let foo_bar_tid = typestore.get_enum_variant(foo_tid, 0, &META).unwrap();
        let foo_baz_tid = typestore.get_enum_variant(foo_tid, 1, &META).unwrap();

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let option_def =
            EnumDef::new(&typestore, vec![t_tyvar], vec![Some(t_tid), None], &META).unwrap();
        let (option_tid, _) = typestore.add_enum(option_def);

        let u8_tid = typestore.intern(Type::u8());
        let option_u8_tid = typestore
            .type_instance(&env, option_tid, Some(vec![u8_tid]), &META)
            .unwrap();

        let option_some_tid = typestore.get_enum_variant(option_tid, 0, &META).unwrap();
        let option_some_ty_tid = typestore
            .metatype(&env, option_some_tid, None, &META)
            .unwrap();
        let option_none_tid = typestore.get_enum_variant(option_tid, 1, &META).unwrap();
        let option_none_ty_tid = typestore
            .metatype(&env, option_none_tid, None, &META)
            .unwrap();

        let test = Function::new(
            [foo_tid],
            [option_u8_tid],
            [],
            [],
            Expr::match_(
                MatchExpr::new(
                    [
                        MatchCase::new(
                            MatchPattern::Variant(foo_bar_tid),
                            Expr::block(
                                [
                                    Expr::u8(0, ()),
                                    Expr::typ(option_some_ty_tid, ()),
                                    Expr::pack(()),
                                ],
                                (),
                            ),
                        ),
                        MatchCase::new(
                            MatchPattern::Variant(foo_baz_tid),
                            Expr::block([Expr::typ(option_none_ty_tid, ()), Expr::pack(())], ()),
                        ),
                    ],
                    None,
                ),
                (),
            ),
        );

        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn match_supertyping_pointers() {
        // enum Foo { u8: Bar u8: Baz }
        // fn test(Foo) -> [&u8] {
        //     match {
        //         Foo::Bar { as [v] &mut v }
        //         Foo::Baz { as [v] &v }
        //     }
        // }

        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let u8_tid = typestore.intern(Type::u8());
        let u8_const_ptr_tid = typestore.pointer(u8_tid, Mutability::Const);

        let foo_def =
            EnumDef::new(&typestore, vec![], vec![Some(u8_tid), Some(u8_tid)], &META).unwrap();
        let (foo_tid, _) = typestore.add_enum(foo_def);
        let foo_bar_tid = typestore.get_enum_variant(foo_tid, 0, &META).unwrap();
        let foo_baz_tid = typestore.get_enum_variant(foo_tid, 1, &META).unwrap();

        let test = Function::new(
            [foo_tid],
            [u8_const_ptr_tid],
            [],
            [],
            Expr::match_(
                MatchExpr::new(
                    [
                        MatchCase::new(
                            MatchPattern::Variant(foo_bar_tid),
                            Expr::block(
                                [
                                    Expr::bind(Var(0), META),
                                    Expr::take_ref(Place::ident(Var(0)), Mutability::Mutable, META),
                                ],
                                META,
                            ),
                        ),
                        MatchCase::new(
                            MatchPattern::Variant(foo_baz_tid),
                            Expr::block(
                                [
                                    Expr::bind(Var(0), META),
                                    Expr::take_ref(Place::ident(Var(0)), Mutability::Const, META),
                                ],
                                META,
                            ),
                        ),
                    ],
                    None,
                ),
                (),
            ),
        );
        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());

        // Test the inverse to ensure order doesn't matter.
        let test = Function::new(
            [foo_tid],
            [u8_const_ptr_tid],
            [],
            [],
            Expr::match_(
                MatchExpr::new(
                    [
                        MatchCase::new(
                            MatchPattern::Variant(foo_bar_tid),
                            Expr::block(
                                [
                                    Expr::bind(Var(0), META),
                                    Expr::take_ref(Place::ident(Var(0)), Mutability::Const, META),
                                ],
                                META,
                            ),
                        ),
                        MatchCase::new(
                            MatchPattern::Variant(foo_baz_tid),
                            Expr::block(
                                [
                                    Expr::bind(Var(0), META),
                                    Expr::take_ref(Place::ident(Var(0)), Mutability::Mutable, META),
                                ],
                                META,
                            ),
                        ),
                    ],
                    None,
                ),
                (),
            ),
        );
        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    fn option_def(typestore: &mut TypeStore<()>) -> (TypeId, TypeId, TypeId) {
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let def = EnumDef::new(&typestore, vec![t_tyvar], vec![Some(t_tid), None], &META).unwrap();
        let (option_tid, _) = typestore.add_enum(def);
        let some_tid = typestore.get_enum_variant(option_tid, 0, &META).unwrap();
        let none_tid = typestore.get_enum_variant(option_tid, 1, &META).unwrap();

        (option_tid, some_tid, none_tid)
    }

    #[test]
    fn match_nested_supertyping() {
        // fn foo(Foo) -> [Option::Some<Option<u8>>] match {
        //     Foo::Bar {   #Option::None<u8> pack #Option::Some pack }
        //     Foo::Baz { 0 #Option::Some     pack #Option::Some pack }
        // }

        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let foo_def = EnumDef::new(&typestore, vec![], vec![None, None], &META).unwrap();
        let (foo_tid, _) = typestore.add_enum(foo_def);
        let foo_bar = typestore.get_enum_variant(foo_tid, 0, &META).unwrap();
        let foo_baz = typestore.get_enum_variant(foo_tid, 1, &META).unwrap();

        let (option, some, none) = option_def(&mut typestore);
        let u8_tid = typestore.intern(Type::u8());
        let option_u8 = typestore
            .type_instance(&env, option, Some(vec![u8_tid]), &META)
            .unwrap();
        let option_some_option_u8 = typestore
            .type_instance(&env, some, Some(vec![option_u8]), &META)
            .unwrap();

        let test = Function::new(
            [foo_tid],
            [option_some_option_u8],
            [],
            [],
            Expr::match_(
                MatchExpr::new(
                    [
                        MatchCase::new(
                            MatchPattern::Variant(foo_bar),
                            Expr::block(
                                [
                                    Expr::typ(
                                        typestore
                                            .metatype(&env, none, Some(vec![u8_tid]), &META)
                                            .unwrap(),
                                        (),
                                    ),
                                    Expr::pack(()),
                                    Expr::typ(
                                        typestore.metatype(&env, some, None, &META).unwrap(),
                                        (),
                                    ),
                                    Expr::pack(()),
                                ],
                                (),
                            ),
                        ),
                        MatchCase::new(
                            MatchPattern::Variant(foo_baz),
                            Expr::block(
                                [
                                    Expr::u8(0, ()),
                                    Expr::typ(
                                        typestore.metatype(&env, some, None, &META).unwrap(),
                                        (),
                                    ),
                                    Expr::pack(()),
                                    Expr::typ(
                                        typestore.metatype(&env, some, None, &META).unwrap(),
                                        (),
                                    ),
                                    Expr::pack(()),
                                ],
                                (),
                            ),
                        ),
                    ],
                    None,
                ),
                (),
            ),
        );

        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn associated_type_basic() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        // interface Foo<A> {
        //     type: B
        //     fn bar(A) -> [B]
        // }
        let a_tyvar = typestore.fresh_type_var();
        let foo = Interface {
            vars: vec![a_tyvar],
            coherence: Coherence::Unique,
            associated_types_count: 1,
        };

        let foo_cid = env.add_typeclass(foo);

        // fn bar(A) -> [B]
        let bar = Var(0);
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::Projection(Projection {
            cid: foo_cid,
            args: vec![a_tid],
            idx: 0,
        }));

        let bar_tid = typestore.function(FunctionType {
            input: vec![a_tid],
            output: vec![b_tid].into(),
        });
        let bar_scheme = Scheme::new(
            [a_tyvar],
            bar_tid,
            [TypePredicate {
                cid: foo_cid,
                args: vec![a_tid],
            }],
        );
        env.bind_scheme(bar, bar_scheme);

        // impl Foo<u8> {
        //     bool: B
        //     fn bar(u8) -> [u8]
        let u8_tid = typestore.intern(Type::u8());
        let bool_tid = typestore.intern(Type::bool());
        assert!(env
            .add_impl(
                &mut typestore,
                foo_cid,
                InterfaceImpl {
                    cid: foo_cid,
                    vars: vec![],
                    args: vec![u8_tid],
                    associated_types: vec![bool_tid],
                    predicates: vec![],
                    coherence: Coherence::Unique,
                },
                &META
            )
            .is_ok());

        let test = Function::new([u8_tid], [bool_tid], [], [], Expr::ident(bar, ()));
        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn associated_type_generic() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        // interface Extract<T> {
        //     type: Inner
        //     fn extract(T) -> [Inner]
        // }
        let t_tyvar = typestore.fresh_type_var();
        let extract = Interface {
            vars: vec![t_tyvar],
            coherence: Coherence::Unique,
            associated_types_count: 1,
        };

        let extract_cid = env.add_typeclass(extract);

        // fn bar(T) -> [inner]
        let extract = Var(0);
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let inner_tid = typestore.intern(Type::Projection(Projection {
            cid: extract_cid,
            args: vec![t_tid],
            idx: 0,
        }));

        let extract_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![inner_tid].into(),
        });
        let extract_scheme = Scheme::new(
            [t_tyvar],
            extract_tid,
            [TypePredicate {
                cid: extract_cid,
                args: vec![t_tid],
            }],
        );
        env.bind_scheme(extract, extract_scheme);

        // impl<T> Extract<&T> {
        //     T: Inner
        //     fn extract(&T) -> [T]
        // }
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let t_ptr_tid = typestore.pointer(t_tid, Mutability::Const);
        assert!(env
            .add_impl(
                &mut typestore,
                extract_cid,
                InterfaceImpl {
                    cid: extract_cid,
                    vars: vec![t_tyvar],
                    args: vec![t_ptr_tid],
                    associated_types: vec![t_tid],
                    predicates: vec![],
                    coherence: Coherence::Unique,
                },
                &META
            )
            .is_ok());

        let u8_tid = typestore.intern(Type::u8());
        let u8_ptr_tid = typestore.pointer(u8_tid, Mutability::Const);
        let bool_tid = typestore.intern(Type::bool());
        let bool_ptr_tid = typestore.pointer(bool_tid, Mutability::Const);

        let test = Function::new([u8_ptr_tid], [u8_tid], [], [], Expr::ident(extract, ()));
        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());
        let test = Function::new([bool_ptr_tid], [bool_tid], [], [], Expr::ident(extract, ()));
        assert!(test.type_check(&mut typestore, &env, &LOOKUP).is_ok());
    }

    #[test]
    fn associated_type_failed_candidate_does_not_affect_next() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        // interface Extract<T, Tag> {
        //     type Inner
        // }
        let t_tyvar = typestore.fresh_type_var();
        let tag_tyvar = typestore.fresh_type_var();

        let extract_cid = env.add_typeclass(Interface {
            vars: vec![t_tyvar, tag_tyvar],
            coherence: Coherence::Unique,
            associated_types_count: 1,
        });

        let u8_tid = typestore.intern(Type::u8());
        let u64_tid = typestore.intern(Type::u64());
        let bool_tid = typestore.intern(Type::bool());

        // impl<T> Extract<&T, u64> {
        //     type Inner = u64;
        // }
        //
        // This candidate partially unifies with the query:
        //     &T  == &u8    succeeds, binding T = u8
        //     u64 == bool   fails
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let t_ptr_tid = typestore.pointer(t_tid, Mutability::Const);

        assert!(env
            .add_impl(
                &mut typestore,
                extract_cid,
                InterfaceImpl {
                    cid: extract_cid,
                    vars: vec![t_tyvar],
                    args: vec![t_ptr_tid, u64_tid],
                    associated_types: vec![u64_tid],
                    predicates: vec![],
                    coherence: Coherence::Unique,
                },
                &META,
            )
            .is_ok());

        // impl<U> Extract<&U, bool> {
        //     type Inner = U;
        // }
        //
        // This candidate should match and resolve Inner = u8.
        let u_tyvar = typestore.fresh_type_var();
        let u_tid = typestore.intern(Type::TypeVar(u_tyvar));
        let u_ptr_tid = typestore.pointer(u_tid, Mutability::Const);

        assert!(env
            .add_impl(
                &mut typestore,
                extract_cid,
                InterfaceImpl {
                    cid: extract_cid,
                    vars: vec![u_tyvar],
                    args: vec![u_ptr_tid, bool_tid],
                    associated_types: vec![u_tid],
                    predicates: vec![],
                    coherence: Coherence::Unique,
                },
                &META,
            )
            .is_ok());

        // Resolve:
        //     Extract<&u8, bool>::Inner
        //
        // Expected: u8
        let u8_ptr_tid = typestore.pointer(u8_tid, Mutability::Const);
        let projection_tid = typestore.intern(Type::Projection(Projection {
            cid: extract_cid,
            args: vec![u8_ptr_tid, bool_tid],
            idx: 0,
        }));

        let (_, resolved_tid) = typestore
            .normalize_ty(&env, projection_tid, &META)
            .expect("Associated type should resolve");

        assert_eq!(resolved_tid, u8_tid);
    }
}

#[cfg(test)]
mod variance_tests {

    use std::{assert_eq, env};

    use crate::typecheck::{
        constraint::ConstraintOrigin,
        environment::{Environment, Variance},
        typ::{
            typ::{EnumDef, FunctionType, Mutability, Projection, StructDef, Type},
            type_id::TypeId,
        },
        typestore::{inference::InferenceError, TypeStore},
    };

    const META: () = ();
    const ORIGIN: ConstraintOrigin = ConstraintOrigin::ConflictingImpls;

    fn animal_def(typestore: &mut TypeStore<()>) -> (TypeId, TypeId) {
        let animal = EnumDef::new(&typestore, vec![], vec![None], &META).unwrap();
        let (animal_tid, _) = typestore.add_enum(animal);
        let dog_tid = typestore.get_enum_variant(animal_tid, 0, &META).unwrap();
        (animal_tid, dog_tid)
    }

    fn option_def(typestore: &mut TypeStore<()>) -> (TypeId, TypeId, TypeId) {
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let def = EnumDef::new(&typestore, vec![t_tyvar], vec![Some(t_tid), None], &META).unwrap();
        let (option_tid, _) = typestore.add_enum(def);
        let some_tid = typestore.get_enum_variant(option_tid, 0, &META).unwrap();
        let none_tid = typestore.get_enum_variant(option_tid, 1, &META).unwrap();

        (option_tid, some_tid, none_tid)
    }

    #[test]
    fn const_ptr_const_ptr_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Dog <: Animal
        // -------------
        // &Dog <: &Animal

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let dog_ptr = typestore.pointer(dog_tid, Mutability::Const);

        assert!(typestore
            .check_subtype(&env, dog_ptr, animal_ptr, &ORIGIN, &META)
            .is_ok());
    }

    #[test]
    fn mut_ptr_const_ptr_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Dog <: Animal
        // -------------
        // &mut Dog <: &Animal

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let dog_mut_ptr = typestore.pointer(dog_tid, Mutability::Mutable);

        assert!(typestore
            .check_subtype(&env, dog_mut_ptr, animal_ptr, &ORIGIN, &META)
            .is_ok());
    }

    #[test]
    fn const_ptr_mut_ptr_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Dog <: Animal
        // -------------
        // &Dog </: &mut Animal

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_mut_ptr = typestore.pointer(animal_tid, Mutability::Mutable);
        let dog_ptr = typestore.pointer(dog_tid, Mutability::Const);

        assert_eq!(
            typestore.check_subtype(&env, dog_ptr, animal_mut_ptr, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_mut_ptr,
                dog_ptr,
                &ORIGIN,
                &META
            ))
        );
    }

    #[test]
    fn mut_ptr_mut_ptr_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Dog <: Animal
        // -------------
        // &mut Dog </: &mut Animal

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_mut_ptr = typestore.pointer(animal_tid, Mutability::Mutable);
        let dog_mut_ptr = typestore.pointer(dog_tid, Mutability::Mutable);

        assert_eq!(
            typestore.check_subtype(&env, dog_mut_ptr, animal_mut_ptr, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn mut_ptr_mut_ptr_equal_t_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Animal <: Animal
        // -------------
        // &mut Animal <: &mut Animal

        let animal = EnumDef::new(&typestore, vec![], vec![None], &META).unwrap();
        let (animal_tid, _) = typestore.add_enum(animal);
        let animal_mut_ptr = typestore.pointer(animal_tid, Mutability::Mutable);

        assert!(typestore
            .check_subtype(&env, animal_mut_ptr, animal_mut_ptr, &ORIGIN, &META)
            .is_ok());
    }

    #[test]
    fn const_ptr_nested_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Dog <: Animal
        // -------------
        // &mut Dog <: &Animal
        // ---------------
        // &&mut Dog <: &&Animal

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let animal_ptr_ptr = typestore.pointer(animal_ptr, Mutability::Const);
        let dog_ptr_mut = typestore.pointer(dog_tid, Mutability::Mutable);
        let dog_ptr_mut_ptr = typestore.pointer(dog_ptr_mut, Mutability::Const);

        assert!(typestore
            .check_subtype(&env, dog_ptr_mut_ptr, animal_ptr_ptr, &ORIGIN, &META)
            .is_ok());
    }

    #[test]
    fn mut_ptr_nested_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Dog <: Animal
        // -------------
        // &mut Dog </: &mut Animal
        // ---------------
        // &&mut Dog </: &&mut Animal

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr_mut = typestore.pointer(animal_tid, Mutability::Mutable);
        let animal_ptr_mut_ptr = typestore.pointer(animal_ptr_mut, Mutability::Const);
        let dog_ptr_mut = typestore.pointer(dog_tid, Mutability::Mutable);
        let dog_ptr_mut_ptr = typestore.pointer(dog_ptr_mut, Mutability::Const);

        assert_eq!(
            typestore.check_subtype(&env, dog_ptr_mut_ptr, animal_ptr_mut_ptr, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn const_sub_ptr_const_super_ptr_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Animal >: Dog
        // -------------
        // &Animal >: &Dog

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let dog_ptr = typestore.pointer(dog_tid, Mutability::Const);

        assert_eq!(
            typestore.join_ty_ty(&env, dog_ptr, animal_ptr, &ORIGIN, &META),
            Ok(animal_ptr)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, animal_ptr, dog_ptr, &ORIGIN, &META),
            Ok(animal_ptr)
        );
    }

    #[test]
    fn mut_sub_ptr_const_super_ptr_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Animal >: Dog
        // -------------
        // &Animal >: &mut Dog

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let dog_mut_ptr = typestore.pointer(dog_tid, Mutability::Mutable);

        assert_eq!(
            typestore.join_ty_ty(&env, dog_mut_ptr, animal_ptr, &ORIGIN, &META),
            Ok(animal_ptr)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, animal_ptr, dog_mut_ptr, &ORIGIN, &META),
            Ok(animal_ptr)
        );
    }

    #[test]
    fn const_sub_ptr_mut_super_ptr_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Animal >: Dog
        // &Animal >: &Dog
        // -------------
        // &Animal >: &mut Dog

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let animal_mut_ptr = typestore.pointer(animal_tid, Mutability::Mutable);
        let dog_ptr = typestore.pointer(dog_tid, Mutability::Const);

        assert_eq!(
            typestore.join_ty_ty(&env, dog_ptr, animal_mut_ptr, &ORIGIN, &META),
            Ok(animal_ptr)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, animal_mut_ptr, dog_ptr, &ORIGIN, &META),
            Ok(animal_ptr)
        );
    }

    #[test]
    fn mut_sub_ptr_mut_super_ptr_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let animal_mut_ptr = typestore.pointer(animal_tid, Mutability::Mutable);
        let dog_mut_ptr = typestore.pointer(dog_tid, Mutability::Mutable);

        assert_eq!(
            typestore.join_ty_ty(&env, dog_mut_ptr, animal_mut_ptr, &ORIGIN, &META),
            Ok(animal_ptr)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, animal_mut_ptr, dog_mut_ptr, &ORIGIN, &META),
            Ok(animal_ptr)
        );
    }

    #[test]
    fn mut_t_ptr_mut_t_ptr_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let animal = EnumDef::new(&typestore, vec![], vec![None], &META).unwrap();
        let (animal_tid, _) = typestore.add_enum(animal);
        let animal_mut_ptr = typestore.pointer(animal_tid, Mutability::Mutable);

        assert_eq!(
            typestore.join_ty_ty(&env, animal_mut_ptr, animal_mut_ptr, &ORIGIN, &META),
            Ok(animal_mut_ptr)
        );
    }

    #[test]
    fn mut_t_ptr_const_t_ptr_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let animal = EnumDef::new(&typestore, vec![], vec![None], &META).unwrap();
        let (animal_tid, _) = typestore.add_enum(animal);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let animal_mut_ptr = typestore.pointer(animal_tid, Mutability::Mutable);

        assert_eq!(
            typestore.join_ty_ty(&env, animal_mut_ptr, animal_ptr, &ORIGIN, &META),
            Ok(animal_ptr)
        );
    }

    #[test]
    fn const_sub_ptr_const_super_ptr_nested_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let animal_ptr_ptr = typestore.pointer(animal_ptr, Mutability::Const);
        let dog_mut_ptr = typestore.pointer(dog_tid, Mutability::Mutable);
        let dog_mut_ptr_ptr = typestore.pointer(dog_mut_ptr, Mutability::Const);

        assert_eq!(
            typestore.join_ty_ty(&env, dog_mut_ptr_ptr, animal_ptr_ptr, &ORIGIN, &META),
            Ok(animal_ptr_ptr)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, animal_ptr_ptr, dog_mut_ptr_ptr, &ORIGIN, &META),
            Ok(animal_ptr_ptr)
        );
    }

    #[test]
    fn mut_sub_ptr_mut_super_ptr_nested_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let animal_ptr_ptr = typestore.pointer(animal_ptr, Mutability::Const);
        let animal_ptr_mut_ptr = typestore.pointer(animal_ptr, Mutability::Mutable);
        let dog_ptr = typestore.pointer(dog_tid, Mutability::Const);
        let dog_ptr_mut_ptr = typestore.pointer(dog_ptr, Mutability::Mutable);

        assert_eq!(
            typestore.join_ty_ty(&env, animal_ptr_mut_ptr, dog_ptr_mut_ptr, &ORIGIN, &META),
            Ok(animal_ptr_ptr)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, dog_ptr_mut_ptr, animal_ptr_mut_ptr, &ORIGIN, &META),
            Ok(animal_ptr_ptr)
        );
    }

    fn covariant_struct(typestore: &mut TypeStore<()>) -> TypeId {
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let def = StructDef::new(&typestore, vec![t_tyvar], vec![t_tid], &META).unwrap();
        assert!(def.variances.iter().all(|v| *v == Variance::Covariant));
        typestore.add_struct(def).0
    }

    fn invariant_struct(typestore: &mut TypeStore<()>) -> TypeId {
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let t_ptr_mut_tid = typestore.pointer(t_tid, Mutability::Mutable);
        let def = StructDef::new(&typestore, vec![t_tyvar], vec![t_ptr_mut_tid], &META).unwrap();
        assert!(def.variances.iter().all(|v| *v == Variance::Invariant));
        typestore.add_struct(def).0
    }

    fn contravariant_struct(typestore: &mut TypeStore<()>) -> TypeId {
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![].into(),
        });

        let def = StructDef::new(&typestore, vec![t_tyvar], vec![f_tid], &META).unwrap();
        assert!(def.variances.iter().all(|v| *v == Variance::Contravariant));
        typestore.add_struct(def).0
    }

    #[test]
    fn covariant_struct_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Dog <: Animal
        // -------------
        // Box<Dog> <: Box<Animal>

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let covariant_struct = covariant_struct(&mut typestore);
        let covariant_struct_animal = typestore
            .type_instance(&env, covariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_struct_dog = typestore
            .type_instance(&env, covariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(
                &env,
                covariant_struct_dog,
                covariant_struct_animal,
                &ORIGIN,
                &META
            )
            .is_ok());
        assert_eq!(
            typestore.check_subtype(
                &env,
                covariant_struct_animal,
                covariant_struct_dog,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn invariant_struct_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Dog <: Animal
        // -------------
        // Invariant<Animal> <:  Invariant<Animal>
        // Invariant<Dog>    </: Invariant<Animal>
        // Invariant<Animal> </: Invariant<Dog>
        // Invariant<Dog>    <:  Invariant<Dog>

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let invariant_struct = invariant_struct(&mut typestore);
        let invariant_struct_animal = typestore
            .type_instance(&env, invariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let invariant_struct_dog = typestore
            .type_instance(&env, invariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(
                &env,
                invariant_struct_animal,
                invariant_struct_animal,
                &ORIGIN,
                &META
            )
            .is_ok());
        assert_eq!(
            typestore.check_subtype(
                &env,
                invariant_struct_animal,
                invariant_struct_dog,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(
                &env,
                invariant_struct_dog,
                invariant_struct_animal,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(
                &env,
                invariant_struct_animal,
                invariant_struct_animal,
                &ORIGIN,
                &META
            )
            .is_ok());
    }

    #[test]
    fn contravariant_struct_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Dog <: Animal
        // -------------
        // Contravariant<Animal> <:  Contravariant<Animal>
        // Contravariant<Dog>    </: Contravariant<Animal>
        // Contravariant<Animal> <: Contravariant<Dog>
        // Contravariant<Dog>    <:  Contravariant<Dog>

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let contravariant_struct = contravariant_struct(&mut typestore);
        let contravariant_struct_animal = typestore
            .type_instance(&env, contravariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_struct_dog = typestore
            .type_instance(&env, contravariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(
                &env,
                contravariant_struct_animal,
                contravariant_struct_animal,
                &ORIGIN,
                &META
            )
            .is_ok());
        assert_eq!(
            typestore.check_subtype(
                &env,
                contravariant_struct_dog,
                contravariant_struct_animal,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(
                &env,
                contravariant_struct_animal,
                contravariant_struct_dog,
                &ORIGIN,
                &META
            )
            .is_ok());
        assert!(typestore
            .check_subtype(
                &env,
                contravariant_struct_dog,
                contravariant_struct_dog,
                &ORIGIN,
                &META
            )
            .is_ok());
    }

    #[test]
    fn nested_covariant_covaraint_struct_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        // -------------
        // Covariant<Dog> <: Covariant<Animal>
        // -----------------------------------
        // Covariant<Covariant<Dog>>    <:  Covariant<Covariant<Animal>>
        // Covariant<Covariant<Animal>> </: Covariant<Covariant<Dog>>

        let covariant_struct = covariant_struct(&mut typestore);
        let covariant_dog = typestore
            .type_instance(&env, covariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let covariant_covariant_dog = typestore
            .type_instance(&env, covariant_struct, Some(vec![covariant_dog]), &META)
            .unwrap();
        let covariant_animal = typestore
            .type_instance(&env, covariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_covariant_animal = typestore
            .type_instance(&env, covariant_struct, Some(vec![covariant_animal]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(
                &env,
                covariant_covariant_dog,
                covariant_covariant_animal,
                &ORIGIN,
                &META
            )
            .is_ok());
        assert_eq!(
            typestore.check_subtype(
                &env,
                covariant_covariant_animal,
                covariant_covariant_dog,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn nested_covariant_contravariant_struct_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        // -------------
        // Covariant<Dog> <: Covariant<Animal>
        // Contravariant<Animal> <: Contravariant<Dog>
        // -----------------------------------
        // Covariant<Contravariant<Dog>>    </: Covariant<Contravariant<Animal>>
        // Covariant<Contravariant<Animal>> <:  Covariant<Contravariant<Dog>>

        let covariant_struct = covariant_struct(&mut typestore);
        let contravariant_struct = contravariant_struct(&mut typestore);
        let contravariant_dog = typestore
            .type_instance(&env, contravariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let covariant_contravariant_dog = typestore
            .type_instance(&env, covariant_struct, Some(vec![contravariant_dog]), &META)
            .unwrap();
        let contravariant_animal = typestore
            .type_instance(&env, contravariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_contravariant_animal = typestore
            .type_instance(
                &env,
                covariant_struct,
                Some(vec![contravariant_animal]),
                &META,
            )
            .unwrap();

        assert_eq!(
            typestore.check_subtype(
                &env,
                covariant_contravariant_dog,
                covariant_contravariant_animal,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(
                &env,
                covariant_contravariant_animal,
                covariant_contravariant_dog,
                &ORIGIN,
                &META
            )
            .is_ok());
    }

    #[test]
    fn nested_contravariant_covariant_struct_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        // -------------
        // Covariant<Dog> <: Covariant<Animal>
        // Contravariant<Animal> <: Contravariant<Dog>
        // -----------------------------------
        // Contravariant<Covariant<Dog>>    </: Contravariant<Covariant<Animal>>
        // Contravariant<Covariant<Animal>> <: Contravariant<Covariant<Dog>>

        let covariant_struct = covariant_struct(&mut typestore);
        let contravariant_struct = contravariant_struct(&mut typestore);
        let covariant_dog = typestore
            .type_instance(&env, covariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let contravariant_covariant_dog = typestore
            .type_instance(&env, contravariant_struct, Some(vec![covariant_dog]), &META)
            .unwrap();
        let covariant_animal = typestore
            .type_instance(&env, covariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_covariant_animal = typestore
            .type_instance(
                &env,
                contravariant_struct,
                Some(vec![covariant_animal]),
                &META,
            )
            .unwrap();

        assert_eq!(
            typestore.check_subtype(
                &env,
                contravariant_covariant_dog,
                contravariant_covariant_animal,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(
                &env,
                contravariant_covariant_animal,
                contravariant_covariant_dog,
                &ORIGIN,
                &META
            )
            .is_ok());
    }

    #[test]
    fn nested_contravariant_contravariant_struct_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        // -------------
        // Contravariant<Animal> <: Contravariant<Dog>
        // -----------------------------------
        // Contravariant<Contravariant<Dog>>    <:  Contravariant<Contravariant<Animal>>
        // Contravariant<Contravariant<Animal>> </: Contravariant<Contravariant<Dog>>

        let contravariant_struct = contravariant_struct(&mut typestore);
        let contravariant_dog = typestore
            .type_instance(&env, contravariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let contravariant_contravariant_dog = typestore
            .type_instance(
                &env,
                contravariant_struct,
                Some(vec![contravariant_dog]),
                &META,
            )
            .unwrap();
        let contravariant_animal = typestore
            .type_instance(&env, contravariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_contravariant_animal = typestore
            .type_instance(
                &env,
                contravariant_struct,
                Some(vec![contravariant_animal]),
                &META,
            )
            .unwrap();

        assert!(typestore
            .check_subtype(
                &env,
                contravariant_contravariant_dog,
                contravariant_contravariant_animal,
                &ORIGIN,
                &META
            )
            .is_ok());
        assert_eq!(
            typestore.check_subtype(
                &env,
                contravariant_contravariant_animal,
                contravariant_contravariant_dog,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn nested_invariant_struct_subtype() {
        // Covariant<Invariant>     => Invariant
        // Contravariant<Invariant> => Invariant
        // Invariant<Covariant>     => Invariant
        // Invariant<Contravariant> => Invariant
        // Invariant<Invariant>     => Invariant

        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let contravariant_struct = contravariant_struct(&mut typestore);
        let covariant_struct = covariant_struct(&mut typestore);
        let invariant_struct = invariant_struct(&mut typestore);

        let invariant_dog = typestore
            .type_instance(&env, invariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let covariant_dog = typestore
            .type_instance(&env, covariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let contravariant_dog = typestore
            .type_instance(&env, contravariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let invariant_animal = typestore
            .type_instance(&env, invariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_animal = typestore
            .type_instance(&env, covariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_animal = typestore
            .type_instance(&env, contravariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();

        let co_inv_dog = typestore
            .type_instance(&env, covariant_struct, Some(vec![invariant_dog]), &META)
            .unwrap();
        let co_inv_animal = typestore
            .type_instance(&env, covariant_struct, Some(vec![invariant_animal]), &META)
            .unwrap();
        let contra_inv_dog = typestore
            .type_instance(&env, contravariant_struct, Some(vec![invariant_dog]), &META)
            .unwrap();
        let contra_inv_animal = typestore
            .type_instance(
                &env,
                contravariant_struct,
                Some(vec![invariant_animal]),
                &META,
            )
            .unwrap();
        let inv_co_dog = typestore
            .type_instance(&env, invariant_struct, Some(vec![covariant_dog]), &META)
            .unwrap();
        let inv_co_animal = typestore
            .type_instance(&env, invariant_struct, Some(vec![covariant_animal]), &META)
            .unwrap();
        let inv_contra_dog = typestore
            .type_instance(&env, invariant_struct, Some(vec![contravariant_dog]), &META)
            .unwrap();
        let inv_contra_animal = typestore
            .type_instance(
                &env,
                invariant_struct,
                Some(vec![contravariant_animal]),
                &META,
            )
            .unwrap();
        let inv_inv_dog = typestore
            .type_instance(&env, invariant_struct, Some(vec![invariant_dog]), &META)
            .unwrap();
        let inv_inv_animal = typestore
            .type_instance(&env, invariant_struct, Some(vec![invariant_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.check_subtype(&env, co_inv_dog, co_inv_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(&env, co_inv_animal, co_inv_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(&env, co_inv_dog, co_inv_dog, &ORIGIN, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(&env, co_inv_animal, co_inv_animal, &ORIGIN, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(&env, contra_inv_dog, contra_inv_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(&env, contra_inv_animal, contra_inv_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(&env, contra_inv_dog, contra_inv_dog, &ORIGIN, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(&env, contra_inv_animal, contra_inv_animal, &ORIGIN, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(&env, inv_co_dog, inv_co_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(&env, inv_co_animal, inv_co_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(&env, inv_co_dog, inv_co_dog, &ORIGIN, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(&env, inv_co_animal, inv_co_animal, &ORIGIN, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(&env, inv_contra_dog, inv_contra_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(&env, inv_contra_animal, inv_contra_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(&env, inv_contra_dog, inv_contra_dog, &ORIGIN, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(&env, inv_contra_animal, inv_contra_animal, &ORIGIN, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(&env, inv_inv_dog, inv_inv_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(&env, inv_inv_animal, inv_inv_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(&env, inv_inv_dog, inv_inv_dog, &ORIGIN, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(&env, inv_inv_animal, inv_inv_animal, &ORIGIN, &META)
            .is_ok());
    }

    #[test]
    fn mixed_variance_struct_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        // Contravariant on `B`
        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![].into(),
        });

        let def = StructDef::new(
            &typestore,
            vec![a_tyvar, b_tyvar],
            vec![a_tid, f_tid],
            &META,
        )
        .unwrap();
        assert!(def.variances[0] == Variance::Covariant);
        assert!(def.variances[1] == Variance::Contravariant);
        let (mixed, _) = typestore.add_struct(def);

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let mixed_dog_animal = typestore
            .type_instance(&env, mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();
        let mixed_animal_dog = typestore
            .type_instance(&env, mixed, Some(vec![animal_tid, dog_tid]), &META)
            .unwrap();
        let mixed_dog_dog = typestore
            .type_instance(&env, mixed, Some(vec![dog_tid, dog_tid]), &META)
            .unwrap();
        let mixed_animal_animal = typestore
            .type_instance(&env, mixed, Some(vec![animal_tid, animal_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(&env, mixed_dog_animal, mixed_animal_dog, &ORIGIN, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(&env, mixed_animal_dog, mixed_dog_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(&env, mixed_dog_dog, mixed_animal_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(&env, mixed_dog_animal, mixed_animal_animal, &ORIGIN, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(&env, mixed_dog_animal, mixed_dog_dog, &ORIGIN, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(&env, mixed_dog_dog, mixed_dog_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(&env, mixed_dog_animal, mixed_dog_dog, &ORIGIN, &META)
            .is_ok());
    }

    #[test]
    fn different_structs_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let a = covariant_struct(&mut typestore);
        let b = contravariant_struct(&mut typestore);

        assert_eq!(
            typestore.check_subtype(&env, a, b, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(b, a, &ORIGIN, &META))
        )
    }

    #[test]
    fn covariant_struct_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let covariant_struct = covariant_struct(&mut typestore);
        let box_animal = typestore
            .type_instance(&env, covariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let box_dog = typestore
            .type_instance(&env, covariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, box_animal, box_dog, &ORIGIN, &META),
            Ok(box_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, box_dog, box_animal, &ORIGIN, &META),
            Ok(box_animal)
        );
    }

    #[test]
    fn invariant_struct_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let invariant_struct = invariant_struct(&mut typestore);
        let inv_animal = typestore
            .type_instance(&env, invariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let inv_dog = typestore
            .type_instance(&env, invariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, inv_animal, inv_animal, &ORIGIN, &META),
            Ok(inv_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, inv_dog, inv_dog, &ORIGIN, &META),
            Ok(inv_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, inv_animal, inv_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.join_ty_ty(&env, inv_dog, inv_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn contravariant_struct_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let contravariant_struct = contravariant_struct(&mut typestore);
        let contra_animal = typestore
            .type_instance(&env, contravariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let contra_dog = typestore
            .type_instance(&env, contravariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, contra_animal, contra_animal, &ORIGIN, &META),
            Ok(contra_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, contra_dog, contra_dog, &ORIGIN, &META),
            Ok(contra_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, contra_animal, contra_dog, &ORIGIN, &META),
            Ok(contra_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, contra_dog, contra_animal, &ORIGIN, &META),
            Ok(contra_dog)
        );
    }

    #[test]
    fn nested_covariant_covariant_struct_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        //
        // Co<Dog> <: Co<Animal>
        //
        // join(
        //     Co<Co<Dog>>,
        //     Co<Co<Animal>>
        // )
        // = Co<Co<Animal>>

        let covariant_struct = covariant_struct(&mut typestore);

        let co_dog = typestore
            .type_instance(&env, covariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let co_animal = typestore
            .type_instance(&env, covariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();

        let co_co_dog = typestore
            .type_instance(&env, covariant_struct, Some(vec![co_dog]), &META)
            .unwrap();
        let co_co_animal = typestore
            .type_instance(&env, covariant_struct, Some(vec![co_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, co_co_dog, co_co_animal, &ORIGIN, &META),
            Ok(co_co_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, co_co_animal, co_co_dog, &ORIGIN, &META),
            Ok(co_co_animal)
        );
    }

    #[test]
    fn nested_covariant_contravariant_struct_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        //
        // Contra<Animal> <: Contra<Dog>
        //
        // Co preserves that relation:
        //
        // Co<Contra<Animal>> <: Co<Contra<Dog>>
        //
        // therefore:
        //
        // join(
        //     Co<Contra<Dog>>,
        //     Co<Contra<Animal>>
        // )
        // = Co<Contra<Dog>>

        let covariant_struct = covariant_struct(&mut typestore);
        let contravariant_struct = contravariant_struct(&mut typestore);

        let contra_dog = typestore
            .type_instance(&env, contravariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let contra_animal = typestore
            .type_instance(&env, contravariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();

        let co_contra_dog = typestore
            .type_instance(&env, covariant_struct, Some(vec![contra_dog]), &META)
            .unwrap();
        let co_contra_animal = typestore
            .type_instance(&env, covariant_struct, Some(vec![contra_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, co_contra_dog, co_contra_animal, &ORIGIN, &META),
            Ok(co_contra_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, co_contra_animal, co_contra_dog, &ORIGIN, &META),
            Ok(co_contra_dog)
        );
    }

    #[test]
    fn nested_contravariant_covariant_struct_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        //
        // Co<Dog> <: Co<Animal>
        //
        // Contra reverses that relation:
        //
        // Contra<Co<Animal>> <: Contra<Co<Dog>>
        //
        // therefore:
        //
        // join(
        //     Contra<Co<Dog>>,
        //     Contra<Co<Animal>>
        // )
        // = Contra<Co<Dog>>

        let covariant_struct = covariant_struct(&mut typestore);
        let contravariant_struct = contravariant_struct(&mut typestore);

        let co_dog = typestore
            .type_instance(&env, covariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let co_animal = typestore
            .type_instance(&env, covariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();

        let contra_co_dog = typestore
            .type_instance(&env, contravariant_struct, Some(vec![co_dog]), &META)
            .unwrap();
        let contra_co_animal = typestore
            .type_instance(&env, contravariant_struct, Some(vec![co_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, contra_co_dog, contra_co_animal, &ORIGIN, &META),
            Ok(contra_co_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, contra_co_animal, contra_co_dog, &ORIGIN, &META),
            Ok(contra_co_dog)
        );
    }

    #[test]
    fn nested_contravariant_contravariant_struct_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        //
        // Contra<Animal> <: Contra<Dog>
        //
        // Applying another Contra reverses it again:
        //
        // Contra<Contra<Dog>> <: Contra<Contra<Animal>>
        //
        // therefore:
        //
        // join(
        //     Contra<Contra<Dog>>,
        //     Contra<Contra<Animal>>
        // )
        // = Contra<Contra<Animal>>

        let contravariant_struct = contravariant_struct(&mut typestore);

        let contra_dog = typestore
            .type_instance(&env, contravariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let contra_animal = typestore
            .type_instance(&env, contravariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();

        let contra_contra_dog = typestore
            .type_instance(&env, contravariant_struct, Some(vec![contra_dog]), &META)
            .unwrap();
        let contra_contra_animal = typestore
            .type_instance(&env, contravariant_struct, Some(vec![contra_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(
                &env,
                contra_contra_dog,
                contra_contra_animal,
                &ORIGIN,
                &META
            ),
            Ok(contra_contra_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(
                &env,
                contra_contra_animal,
                contra_contra_dog,
                &ORIGIN,
                &META
            ),
            Ok(contra_contra_animal)
        );
    }

    #[test]
    fn mixed_variance_struct_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        // A is covariant, B is contravariant.
        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![].into(),
        });

        let def = StructDef::new(
            &typestore,
            vec![a_tyvar, b_tyvar],
            vec![a_tid, f_tid],
            &META,
        )
        .unwrap();

        assert_eq!(def.variances[0], Variance::Covariant);
        assert_eq!(def.variances[1], Variance::Contravariant);

        let (mixed, _) = typestore.add_struct(def);

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Mixed<Dog, Animal> <: Mixed<Animal, Dog>
        //
        // join A:
        //     join(Dog, Animal) = Animal
        //
        // join B (contravariant):
        //     meet(Animal, Dog) = Dog
        //
        // therefore:
        // join(
        //     Mixed<Dog, Animal>,
        //     Mixed<Animal, Dog>
        // )
        // = Mixed<Animal, Dog>

        let mixed_dog_animal = typestore
            .type_instance(&env, mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();

        let mixed_animal_dog = typestore
            .type_instance(&env, mixed, Some(vec![animal_tid, dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_dog_animal, mixed_animal_dog, &ORIGIN, &META),
            Ok(mixed_animal_dog)
        );

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_animal_dog, mixed_dog_animal, &ORIGIN, &META),
            Ok(mixed_animal_dog)
        );
    }

    #[test]
    fn mixed_variance_struct_join_both_arguments() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![].into(),
        });

        let def = StructDef::new(
            &typestore,
            vec![a_tyvar, b_tyvar],
            vec![a_tid, f_tid],
            &META,
        )
        .unwrap();

        let (mixed, _) = typestore.add_struct(def);

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // join(
        //     Mixed<Dog, Dog>,
        //     Mixed<Animal, Animal>
        // )
        //
        // A is covariant:
        //     join(Dog, Animal) = Animal
        //
        // B is contravariant:
        //     meet(Dog, Animal) = Dog
        //
        // result:
        //     Mixed<Animal, Dog>

        let mixed_dog_dog = typestore
            .type_instance(&env, mixed, Some(vec![dog_tid, dog_tid]), &META)
            .unwrap();

        let mixed_animal_animal = typestore
            .type_instance(&env, mixed, Some(vec![animal_tid, animal_tid]), &META)
            .unwrap();

        let mixed_animal_dog = typestore
            .type_instance(&env, mixed, Some(vec![animal_tid, dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_dog_dog, mixed_animal_animal, &ORIGIN, &META),
            Ok(mixed_animal_dog)
        );

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_animal_animal, mixed_dog_dog, &ORIGIN, &META),
            Ok(mixed_animal_dog)
        );
    }

    #[test]
    fn mixed_variance_struct_join_contravariant_argument() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![].into(),
        });

        let def = StructDef::new(
            &typestore,
            vec![a_tyvar, b_tyvar],
            vec![a_tid, f_tid],
            &META,
        )
        .unwrap();

        let (mixed, _) = typestore.add_struct(def);

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // A is identical.
        //
        // join(
        //     Mixed<Dog, Animal>,
        //     Mixed<Dog, Dog>
        // )
        //
        // B is contravariant:
        //     meet(Animal, Dog) = Dog
        //
        // result:
        //     Mixed<Dog, Dog>

        let mixed_dog_animal = typestore
            .type_instance(&env, mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();

        let mixed_dog_dog = typestore
            .type_instance(&env, mixed, Some(vec![dog_tid, dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_dog_animal, mixed_dog_dog, &ORIGIN, &META),
            Ok(mixed_dog_dog)
        );

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_dog_dog, mixed_dog_animal, &ORIGIN, &META),
            Ok(mixed_dog_dog)
        );
    }

    #[test]
    fn mixed_variance_struct_join_covariant_argument() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![].into(),
        });

        let def = StructDef::new(
            &typestore,
            vec![a_tyvar, b_tyvar],
            vec![a_tid, f_tid],
            &META,
        )
        .unwrap();

        let (mixed, _) = typestore.add_struct(def);

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // B is identical.
        //
        // join(
        //     Mixed<Dog, Animal>,
        //     Mixed<Animal, Animal>
        // )
        //
        // A is covariant:
        //     join(Dog, Animal) = Animal
        //
        // result:
        //     Mixed<Animal, Animal>

        let mixed_dog_animal = typestore
            .type_instance(&env, mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();

        let mixed_animal_animal = typestore
            .type_instance(&env, mixed, Some(vec![animal_tid, animal_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_dog_animal, mixed_animal_animal, &ORIGIN, &META),
            Ok(mixed_animal_animal)
        );

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_animal_animal, mixed_dog_animal, &ORIGIN, &META),
            Ok(mixed_animal_animal)
        );
    }

    #[test]
    fn different_structs_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let a = covariant_struct(&mut typestore);
        let b = contravariant_struct(&mut typestore);

        assert_eq!(
            typestore.join_ty_ty(&env, a, b, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(a, b, &ORIGIN, &META))
        )
    }

    fn covariant_enum(typestore: &mut TypeStore<()>) -> TypeId {
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let def = EnumDef::new(&typestore, vec![t_tyvar], vec![Some(t_tid)], &META).unwrap();
        assert!(def.variances.iter().all(|v| *v == Variance::Covariant));
        typestore.add_enum(def).0
    }

    fn invariant_enum(typestore: &mut TypeStore<()>) -> TypeId {
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let t_ptr_mut_tid = typestore.pointer(t_tid, Mutability::Mutable);
        let def =
            EnumDef::new(&typestore, vec![t_tyvar], vec![Some(t_ptr_mut_tid)], &META).unwrap();
        assert!(def.variances.iter().all(|v| *v == Variance::Invariant));
        typestore.add_enum(def).0
    }

    fn contravariant_enum(typestore: &mut TypeStore<()>) -> TypeId {
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![].into(),
        });

        let def = EnumDef::new(&typestore, vec![t_tyvar], vec![Some(f_tid)], &META).unwrap();
        assert!(def.variances.iter().all(|v| *v == Variance::Contravariant));
        typestore.add_enum(def).0
    }

    #[test]
    fn covariant_enum_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Dog <: Animal
        // -------------
        // Box<Dog> <: Box<Animal>

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let covariant_enum = covariant_enum(&mut typestore);
        let covariant_enum_animal = typestore
            .type_instance(&env, covariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_enum_dog = typestore
            .type_instance(&env, covariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(
                &env,
                covariant_enum_dog,
                covariant_enum_animal,
                &ORIGIN,
                &META
            )
            .is_ok());
        assert_eq!(
            typestore.check_subtype(
                &env,
                covariant_enum_animal,
                covariant_enum_dog,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn invariant_enum_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();
        // Dog <: Animal
        // -------------
        // Invariant<Animal> <:  Invariant<Animal>
        // Invariant<Dog>    </: Invariant<Animal>
        // Invariant<Animal> </: Invariant<Dog>
        // Invariant<Dog>    <:  Invariant<Dog>

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let invariant_enum = invariant_enum(&mut typestore);
        let invariant_enum_animal = typestore
            .type_instance(&env, invariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let invariant_enum_dog = typestore
            .type_instance(&env, invariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(
                &env,
                invariant_enum_animal,
                invariant_enum_animal,
                &ORIGIN,
                &META
            )
            .is_ok());
        assert_eq!(
            typestore.check_subtype(
                &env,
                invariant_enum_animal,
                invariant_enum_dog,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(
                &env,
                invariant_enum_dog,
                invariant_enum_animal,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(
                &env,
                invariant_enum_animal,
                invariant_enum_animal,
                &ORIGIN,
                &META
            )
            .is_ok());
    }

    #[test]
    fn contravariant_enum_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        // Dog <: Animal
        // -------------
        // Contravariant<Animal> <:  Contravariant<Animal>
        // Contravariant<Dog>    </: Contravariant<Animal>
        // Contravariant<Animal> <: Contravariant<Dog>
        // Contravariant<Dog>    <:  Contravariant<Dog>

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let contravariant_enum = contravariant_enum(&mut typestore);
        let contravariant_enum_animal = typestore
            .type_instance(&env, contravariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_enum_dog = typestore
            .type_instance(&env, contravariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(
                &env,
                contravariant_enum_animal,
                contravariant_enum_animal,
                &ORIGIN,
                &META
            )
            .is_ok());
        assert_eq!(
            typestore.check_subtype(
                &env,
                contravariant_enum_dog,
                contravariant_enum_animal,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(
                &env,
                contravariant_enum_animal,
                contravariant_enum_dog,
                &ORIGIN,
                &META
            )
            .is_ok());
        assert!(typestore
            .check_subtype(
                &env,
                contravariant_enum_dog,
                contravariant_enum_dog,
                &ORIGIN,
                &META
            )
            .is_ok());
    }

    #[test]
    fn nested_covariant_covaraint_enum_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        // -------------
        // Covariant<Dog> <: Covariant<Animal>
        // -----------------------------------
        // Covariant<Covariant<Dog>>    <:  Covariant<Covariant<Animal>>
        // Covariant<Covariant<Animal>> </: Covariant<Covariant<Dog>>

        let covariant_enum = covariant_enum(&mut typestore);
        let covariant_dog = typestore
            .type_instance(&env, covariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let covariant_covariant_dog = typestore
            .type_instance(&env, covariant_enum, Some(vec![covariant_dog]), &META)
            .unwrap();
        let covariant_animal = typestore
            .type_instance(&env, covariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_covariant_animal = typestore
            .type_instance(&env, covariant_enum, Some(vec![covariant_animal]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(
                &env,
                covariant_covariant_dog,
                covariant_covariant_animal,
                &ORIGIN,
                &META
            )
            .is_ok());
        assert_eq!(
            typestore.check_subtype(
                &env,
                covariant_covariant_animal,
                covariant_covariant_dog,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn nested_covariant_contravariant_enum_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        // -------------
        // Covariant<Dog> <: Covariant<Animal>
        // Contravariant<Animal> <: Contravariant<Dog>
        // -----------------------------------
        // Covariant<Contravariant<Dog>>    </: Covariant<Contravariant<Animal>>
        // Covariant<Contravariant<Animal>> <:  Covariant<Contravariant<Dog>>

        let covariant_enum = covariant_enum(&mut typestore);
        let contravariant_enum = contravariant_enum(&mut typestore);
        let contravariant_dog = typestore
            .type_instance(&env, contravariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let covariant_contravariant_dog = typestore
            .type_instance(&env, covariant_enum, Some(vec![contravariant_dog]), &META)
            .unwrap();
        let contravariant_animal = typestore
            .type_instance(&env, contravariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_contravariant_animal = typestore
            .type_instance(
                &env,
                covariant_enum,
                Some(vec![contravariant_animal]),
                &META,
            )
            .unwrap();

        assert_eq!(
            typestore.check_subtype(
                &env,
                covariant_contravariant_dog,
                covariant_contravariant_animal,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(
                &env,
                covariant_contravariant_animal,
                covariant_contravariant_dog,
                &ORIGIN,
                &META
            )
            .is_ok());
    }

    #[test]
    fn nested_contravariant_covariant_enum_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();
        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        // -------------
        // Covariant<Dog> <: Covariant<Animal>
        // Contravariant<Animal> <: Contravariant<Dog>
        // -----------------------------------
        // Contravariant<Covariant<Dog>>    </: Contravariant<Covariant<Animal>>
        // Contravariant<Covariant<Animal>> <: Contravariant<Covariant<Dog>>

        let covariant_enum = covariant_enum(&mut typestore);
        let contravariant_enum = contravariant_enum(&mut typestore);
        let covariant_dog = typestore
            .type_instance(&env, covariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let contravariant_covariant_dog = typestore
            .type_instance(&env, contravariant_enum, Some(vec![covariant_dog]), &META)
            .unwrap();
        let covariant_animal = typestore
            .type_instance(&env, covariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_covariant_animal = typestore
            .type_instance(
                &env,
                contravariant_enum,
                Some(vec![covariant_animal]),
                &META,
            )
            .unwrap();

        assert_eq!(
            typestore.check_subtype(
                &env,
                contravariant_covariant_dog,
                contravariant_covariant_animal,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(
                &env,
                contravariant_covariant_animal,
                contravariant_covariant_dog,
                &ORIGIN,
                &META
            )
            .is_ok());
    }

    #[test]
    fn nested_contravariant_contravariant_enum_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        // -------------
        // Contravariant<Animal> <: Contravariant<Dog>
        // -----------------------------------
        // Contravariant<Contravariant<Dog>>    <:  Contravariant<Contravariant<Animal>>
        // Contravariant<Contravariant<Animal>> </: Contravariant<Contravariant<Dog>>

        let contravariant_enum = contravariant_enum(&mut typestore);
        let contravariant_dog = typestore
            .type_instance(&env, contravariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let contravariant_contravariant_dog = typestore
            .type_instance(
                &env,
                contravariant_enum,
                Some(vec![contravariant_dog]),
                &META,
            )
            .unwrap();
        let contravariant_animal = typestore
            .type_instance(&env, contravariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_contravariant_animal = typestore
            .type_instance(
                &env,
                contravariant_enum,
                Some(vec![contravariant_animal]),
                &META,
            )
            .unwrap();

        assert!(typestore
            .check_subtype(
                &env,
                contravariant_contravariant_dog,
                contravariant_contravariant_animal,
                &ORIGIN,
                &META
            )
            .is_ok());
        assert_eq!(
            typestore.check_subtype(
                &env,
                contravariant_contravariant_animal,
                contravariant_contravariant_dog,
                &ORIGIN,
                &META
            ),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn nested_invariant_enum_subtype() {
        // Covariant<Invariant>     => Invariant
        // Contravariant<Invariant> => Invariant
        // Invariant<Covariant>     => Invariant
        // Invariant<Contravariant> => Invariant
        // Invariant<Invariant>     => Invariant

        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let contravariant_enum = contravariant_enum(&mut typestore);
        let covariant_enum = covariant_enum(&mut typestore);
        let invariant_enum = invariant_enum(&mut typestore);

        let invariant_dog = typestore
            .type_instance(&env, invariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let covariant_dog = typestore
            .type_instance(&env, covariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let contravariant_dog = typestore
            .type_instance(&env, contravariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let invariant_animal = typestore
            .type_instance(&env, invariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_animal = typestore
            .type_instance(&env, covariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_animal = typestore
            .type_instance(&env, contravariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();

        let co_inv_dog = typestore
            .type_instance(&env, covariant_enum, Some(vec![invariant_dog]), &META)
            .unwrap();
        let co_inv_animal = typestore
            .type_instance(&env, covariant_enum, Some(vec![invariant_animal]), &META)
            .unwrap();
        let contra_inv_dog = typestore
            .type_instance(&env, contravariant_enum, Some(vec![invariant_dog]), &META)
            .unwrap();
        let contra_inv_animal = typestore
            .type_instance(
                &env,
                contravariant_enum,
                Some(vec![invariant_animal]),
                &META,
            )
            .unwrap();
        let inv_co_dog = typestore
            .type_instance(&env, invariant_enum, Some(vec![covariant_dog]), &META)
            .unwrap();
        let inv_co_animal = typestore
            .type_instance(&env, invariant_enum, Some(vec![covariant_animal]), &META)
            .unwrap();
        let inv_contra_dog = typestore
            .type_instance(&env, invariant_enum, Some(vec![contravariant_dog]), &META)
            .unwrap();
        let inv_contra_animal = typestore
            .type_instance(
                &env,
                invariant_enum,
                Some(vec![contravariant_animal]),
                &META,
            )
            .unwrap();
        let inv_inv_dog = typestore
            .type_instance(&env, invariant_enum, Some(vec![invariant_dog]), &META)
            .unwrap();
        let inv_inv_animal = typestore
            .type_instance(&env, invariant_enum, Some(vec![invariant_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.check_subtype(&env, co_inv_dog, co_inv_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(&env, co_inv_animal, co_inv_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(&env, co_inv_dog, co_inv_dog, &ORIGIN, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(&env, co_inv_animal, co_inv_animal, &ORIGIN, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(&env, contra_inv_dog, contra_inv_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(&env, contra_inv_animal, contra_inv_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(&env, contra_inv_dog, contra_inv_dog, &ORIGIN, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(&env, contra_inv_animal, contra_inv_animal, &ORIGIN, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(&env, inv_co_dog, inv_co_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(&env, inv_co_animal, inv_co_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(&env, inv_co_dog, inv_co_dog, &ORIGIN, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(&env, inv_co_animal, inv_co_animal, &ORIGIN, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(&env, inv_contra_dog, inv_contra_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(&env, inv_contra_animal, inv_contra_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(&env, inv_contra_dog, inv_contra_dog, &ORIGIN, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(&env, inv_contra_animal, inv_contra_animal, &ORIGIN, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(&env, inv_inv_dog, inv_inv_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(&env, inv_inv_animal, inv_inv_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(&env, inv_inv_dog, inv_inv_dog, &ORIGIN, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(&env, inv_inv_animal, inv_inv_animal, &ORIGIN, &META)
            .is_ok());
    }

    #[test]
    fn mixed_variance_enum_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        // Contravariant on `B`
        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![].into(),
        });

        let def = EnumDef::new(
            &typestore,
            vec![a_tyvar, b_tyvar],
            vec![Some(a_tid), Some(f_tid)],
            &META,
        )
        .unwrap();
        assert!(def.variances[0] == Variance::Covariant);
        assert!(def.variances[1] == Variance::Contravariant);
        let (mixed, _) = typestore.add_enum(def);

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let mixed_dog_animal = typestore
            .type_instance(&env, mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();
        let mixed_animal_dog = typestore
            .type_instance(&env, mixed, Some(vec![animal_tid, dog_tid]), &META)
            .unwrap();
        let mixed_dog_dog = typestore
            .type_instance(&env, mixed, Some(vec![dog_tid, dog_tid]), &META)
            .unwrap();
        let mixed_animal_animal = typestore
            .type_instance(&env, mixed, Some(vec![animal_tid, animal_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(&env, mixed_dog_animal, mixed_animal_dog, &ORIGIN, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(&env, mixed_animal_dog, mixed_dog_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(&env, mixed_dog_dog, mixed_animal_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(&env, mixed_dog_animal, mixed_animal_animal, &ORIGIN, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(&env, mixed_dog_animal, mixed_dog_dog, &ORIGIN, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(&env, mixed_dog_dog, mixed_dog_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert!(typestore
            .check_subtype(&env, mixed_dog_animal, mixed_dog_dog, &ORIGIN, &META)
            .is_ok());
    }

    #[test]
    fn different_enums_subtype() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();
        let a = covariant_enum(&mut typestore);
        let b = contravariant_enum(&mut typestore);

        assert_eq!(
            typestore.check_subtype(&env, a, b, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(b, a, &ORIGIN, &META))
        )
    }

    #[test]
    fn covariant_enum_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let covariant_enum = covariant_enum(&mut typestore);
        let box_animal = typestore
            .type_instance(&env, covariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let box_dog = typestore
            .type_instance(&env, covariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, box_animal, box_dog, &ORIGIN, &META),
            Ok(box_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, box_dog, box_animal, &ORIGIN, &META),
            Ok(box_animal)
        );
    }

    #[test]
    fn invariant_enum_join() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let invariant_enum = invariant_enum(&mut typestore);
        let inv_animal = typestore
            .type_instance(&env, invariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let inv_dog = typestore
            .type_instance(&env, invariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, inv_animal, inv_animal, &ORIGIN, &META),
            Ok(inv_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, inv_dog, inv_dog, &ORIGIN, &META),
            Ok(inv_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, inv_animal, inv_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.join_ty_ty(&env, inv_dog, inv_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn contravariant_enum_join() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let contravariant_enum = contravariant_enum(&mut typestore);
        let contra_animal = typestore
            .type_instance(&env, contravariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let contra_dog = typestore
            .type_instance(&env, contravariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, contra_animal, contra_animal, &ORIGIN, &META),
            Ok(contra_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, contra_dog, contra_dog, &ORIGIN, &META),
            Ok(contra_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, contra_animal, contra_dog, &ORIGIN, &META),
            Ok(contra_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, contra_dog, contra_animal, &ORIGIN, &META),
            Ok(contra_dog)
        );
    }

    #[test]
    fn nested_covariant_covariant_enum_join() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        //
        // Co<Dog> <: Co<Animal>
        //
        // join(
        //     Co<Co<Dog>>,
        //     Co<Co<Animal>>
        // )
        // = Co<Co<Animal>>

        let covariant_enum = covariant_enum(&mut typestore);

        let co_dog = typestore
            .type_instance(&env, covariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let co_animal = typestore
            .type_instance(&env, covariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();

        let co_co_dog = typestore
            .type_instance(&env, covariant_enum, Some(vec![co_dog]), &META)
            .unwrap();
        let co_co_animal = typestore
            .type_instance(&env, covariant_enum, Some(vec![co_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, co_co_dog, co_co_animal, &ORIGIN, &META),
            Ok(co_co_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, co_co_animal, co_co_dog, &ORIGIN, &META),
            Ok(co_co_animal)
        );
    }

    #[test]
    fn nested_covariant_contravariant_enum_join() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        //
        // Contra<Animal> <: Contra<Dog>
        //
        // Co preserves that relation:
        //
        // Co<Contra<Animal>> <: Co<Contra<Dog>>
        //
        // therefore:
        //
        // join(
        //     Co<Contra<Dog>>,
        //     Co<Contra<Animal>>
        // )
        // = Co<Contra<Dog>>

        let covariant_enum = covariant_enum(&mut typestore);
        let contravariant_enum = contravariant_enum(&mut typestore);

        let contra_dog = typestore
            .type_instance(&env, contravariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let contra_animal = typestore
            .type_instance(&env, contravariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();

        let co_contra_dog = typestore
            .type_instance(&env, covariant_enum, Some(vec![contra_dog]), &META)
            .unwrap();
        let co_contra_animal = typestore
            .type_instance(&env, covariant_enum, Some(vec![contra_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, co_contra_dog, co_contra_animal, &ORIGIN, &META),
            Ok(co_contra_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, co_contra_animal, co_contra_dog, &ORIGIN, &META),
            Ok(co_contra_dog)
        );
    }

    #[test]
    fn nested_contravariant_covariant_enum_join() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        //
        // Co<Dog> <: Co<Animal>
        //
        // Contra reverses that relation:
        //
        // Contra<Co<Animal>> <: Contra<Co<Dog>>
        //
        // therefore:
        //
        // join(
        //     Contra<Co<Dog>>,
        //     Contra<Co<Animal>>
        // )
        // = Contra<Co<Dog>>

        let covariant_enum = covariant_enum(&mut typestore);
        let contravariant_enum = contravariant_enum(&mut typestore);

        let co_dog = typestore
            .type_instance(&env, covariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let co_animal = typestore
            .type_instance(&env, covariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();

        let contra_co_dog = typestore
            .type_instance(&env, contravariant_enum, Some(vec![co_dog]), &META)
            .unwrap();
        let contra_co_animal = typestore
            .type_instance(&env, contravariant_enum, Some(vec![co_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, contra_co_dog, contra_co_animal, &ORIGIN, &META),
            Ok(contra_co_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(&env, contra_co_animal, contra_co_dog, &ORIGIN, &META),
            Ok(contra_co_dog)
        );
    }

    #[test]
    fn nested_contravariant_contravariant_enum_join() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        //
        // Contra<Animal> <: Contra<Dog>
        //
        // Applying another Contra reverses it again:
        //
        // Contra<Contra<Dog>> <: Contra<Contra<Animal>>
        //
        // therefore:
        //
        // join(
        //     Contra<Contra<Dog>>,
        //     Contra<Contra<Animal>>
        // )
        // = Contra<Contra<Animal>>

        let contravariant_enum = contravariant_enum(&mut typestore);

        let contra_dog = typestore
            .type_instance(&env, contravariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let contra_animal = typestore
            .type_instance(&env, contravariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();

        let contra_contra_dog = typestore
            .type_instance(&env, contravariant_enum, Some(vec![contra_dog]), &META)
            .unwrap();
        let contra_contra_animal = typestore
            .type_instance(&env, contravariant_enum, Some(vec![contra_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(
                &env,
                contra_contra_dog,
                contra_contra_animal,
                &ORIGIN,
                &META
            ),
            Ok(contra_contra_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(
                &env,
                contra_contra_animal,
                contra_contra_dog,
                &ORIGIN,
                &META
            ),
            Ok(contra_contra_animal)
        );
    }

    #[test]
    fn mixed_variance_enum_join() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        // A is covariant, B is contravariant.
        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![].into(),
        });

        let def = EnumDef::new(
            &typestore,
            vec![a_tyvar, b_tyvar],
            vec![Some(a_tid), Some(f_tid)],
            &META,
        )
        .unwrap();

        assert_eq!(def.variances[0], Variance::Covariant);
        assert_eq!(def.variances[1], Variance::Contravariant);

        let (mixed, _) = typestore.add_enum(def);

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Mixed<Dog, Animal> <: Mixed<Animal, Dog>
        //
        // join A:
        //     join(Dog, Animal) = Animal
        //
        // join B (contravariant):
        //     meet(Animal, Dog) = Dog
        //
        // therefore:
        // join(
        //     Mixed<Dog, Animal>,
        //     Mixed<Animal, Dog>
        // )
        // = Mixed<Animal, Dog>

        let mixed_dog_animal = typestore
            .type_instance(&env, mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();

        let mixed_animal_dog = typestore
            .type_instance(&env, mixed, Some(vec![animal_tid, dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_dog_animal, mixed_animal_dog, &ORIGIN, &META),
            Ok(mixed_animal_dog)
        );

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_animal_dog, mixed_dog_animal, &ORIGIN, &META),
            Ok(mixed_animal_dog)
        );
    }

    #[test]
    fn mixed_variance_enum_join_both_arguments() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![].into(),
        });

        let def = EnumDef::new(
            &typestore,
            vec![a_tyvar, b_tyvar],
            vec![Some(a_tid), Some(f_tid)],
            &META,
        )
        .unwrap();

        let (mixed, _) = typestore.add_enum(def);

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // join(
        //     Mixed<Dog, Dog>,
        //     Mixed<Animal, Animal>
        // )
        //
        // A is covariant:
        //     join(Dog, Animal) = Animal
        //
        // B is contravariant:
        //     meet(Dog, Animal) = Dog
        //
        // result:
        //     Mixed<Animal, Dog>

        let mixed_dog_dog = typestore
            .type_instance(&env, mixed, Some(vec![dog_tid, dog_tid]), &META)
            .unwrap();

        let mixed_animal_animal = typestore
            .type_instance(&env, mixed, Some(vec![animal_tid, animal_tid]), &META)
            .unwrap();

        let mixed_animal_dog = typestore
            .type_instance(&env, mixed, Some(vec![animal_tid, dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_dog_dog, mixed_animal_animal, &ORIGIN, &META),
            Ok(mixed_animal_dog)
        );

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_animal_animal, mixed_dog_dog, &ORIGIN, &META),
            Ok(mixed_animal_dog)
        );
    }

    #[test]
    fn mixed_variance_enum_join_contravariant_argument() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![].into(),
        });

        let def = EnumDef::new(
            &typestore,
            vec![a_tyvar, b_tyvar],
            vec![Some(a_tid), Some(f_tid)],
            &META,
        )
        .unwrap();

        let (mixed, _) = typestore.add_enum(def);

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // A is identical.
        //
        // join(
        //     Mixed<Dog, Animal>,
        //     Mixed<Dog, Dog>
        // )
        //
        // B is contravariant:
        //     meet(Animal, Dog) = Dog
        //
        // result:
        //     Mixed<Dog, Dog>

        let mixed_dog_animal = typestore
            .type_instance(&env, mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();

        let mixed_dog_dog = typestore
            .type_instance(&env, mixed, Some(vec![dog_tid, dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_dog_animal, mixed_dog_dog, &ORIGIN, &META),
            Ok(mixed_dog_dog)
        );

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_dog_dog, mixed_dog_animal, &ORIGIN, &META),
            Ok(mixed_dog_dog)
        );
    }

    #[test]
    fn mixed_variance_enum_join_covariant_argument() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![].into(),
        });

        let def = EnumDef::new(
            &typestore,
            vec![a_tyvar, b_tyvar],
            vec![Some(a_tid), Some(f_tid)],
            &META,
        )
        .unwrap();

        let (mixed, _) = typestore.add_enum(def);

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // B is identical.
        //
        // join(
        //     Mixed<Dog, Animal>,
        //     Mixed<Animal, Animal>
        // )
        //
        // A is covariant:
        //     join(Dog, Animal) = Animal
        //
        // result:
        //     Mixed<Animal, Animal>

        let mixed_dog_animal = typestore
            .type_instance(&env, mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();

        let mixed_animal_animal = typestore
            .type_instance(&env, mixed, Some(vec![animal_tid, animal_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_dog_animal, mixed_animal_animal, &ORIGIN, &META),
            Ok(mixed_animal_animal)
        );

        assert_eq!(
            typestore.join_ty_ty(&env, mixed_animal_animal, mixed_dog_animal, &ORIGIN, &META),
            Ok(mixed_animal_animal)
        );
    }

    #[test]
    fn different_enums_join() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();
        let a = covariant_enum(&mut typestore);
        let b = contravariant_enum(&mut typestore);

        assert_eq!(
            typestore.join_ty_ty(&env, a, b, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(a, b, &ORIGIN, &META))
        )
    }

    #[test]
    fn same_variants_same_args_subtype() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();
        let (_, dog_tid) = animal_def(&mut typestore);
        let (_, some, _) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(&env, some, Some(vec![dog_tid]), &META)
            .unwrap();
        assert!(typestore
            .check_subtype(&env, some_dog, some_dog, &ORIGIN, &META)
            .is_ok());
    }

    #[test]
    fn same_variants_subtype_args_subtype() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();
        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let (_, some, _) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(&env, some, Some(vec![dog_tid]), &META)
            .unwrap();
        let some_animal = typestore
            .type_instance(&env, some, Some(vec![animal_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(&env, some_dog, some_animal, &ORIGIN, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(&env, some_animal, some_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn different_variants_same_args_subtype() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();
        let (_, dog_tid) = animal_def(&mut typestore);
        let (_, some, none) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(&env, some, Some(vec![dog_tid]), &META)
            .unwrap();
        let none_dog = typestore
            .type_instance(&env, none, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.check_subtype(&env, some_dog, none_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                none_dog, some_dog, &ORIGIN, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(&env, none_dog, some_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                some_dog, none_dog, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn variant_parent_same_args_subtype() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();
        let (_, dog_tid) = animal_def(&mut typestore);
        let (option, some, _) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(&env, some, Some(vec![dog_tid]), &META)
            .unwrap();
        let option_dog = typestore
            .type_instance(&env, option, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(&env, some_dog, option_dog, &ORIGIN, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(&env, option_dog, some_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                some_dog, option_dog, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn variant_parent_subtype_args_subtype() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();
        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let (option, some, _) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(&env, some, Some(vec![dog_tid]), &META)
            .unwrap();

        let some_animal = typestore
            .type_instance(&env, some, Some(vec![animal_tid]), &META)
            .unwrap();

        let option_animal = typestore
            .type_instance(&env, option, Some(vec![animal_tid]), &META)
            .unwrap();

        let option_dog = typestore
            .type_instance(&env, option, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(&env, some_dog, option_animal, &ORIGIN, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(&env, option_animal, some_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                some_dog,
                option_animal,
                &ORIGIN,
                &META
            ))
        );

        assert_eq!(
            typestore.check_subtype(&env, some_animal, option_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn enum_variants_mismatch_subtype() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();
        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let (_, some, _) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(&env, some, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.check_subtype(&env, animal_tid, some_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                some_dog, animal_tid, &ORIGIN, &META
            ))
        )
    }

    #[test]
    fn covariant_variant_subtype() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let enum_tid = covariant_enum(&mut typestore);
        let variant_tid = typestore.get_enum_variant(enum_tid, 0, &META).unwrap();

        let variant_dog = typestore
            .type_instance(&env, variant_tid, Some(vec![dog_tid]), &META)
            .unwrap();

        let variant_animal = typestore
            .type_instance(&env, variant_tid, Some(vec![animal_tid]), &META)
            .unwrap();

        // Dog <: Animal
        // ---------------------------
        // Variant<Dog> <: Variant<Animal>

        assert!(typestore
            .check_subtype(&env, variant_dog, variant_animal, &ORIGIN, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(&env, variant_animal, variant_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn contravariant_variant_subtype() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let enum_tid = contravariant_enum(&mut typestore);
        let variant_tid = typestore.get_enum_variant(enum_tid, 0, &META).unwrap();

        let variant_dog = typestore
            .type_instance(&env, variant_tid, Some(vec![dog_tid]), &META)
            .unwrap();

        let variant_animal = typestore
            .type_instance(&env, variant_tid, Some(vec![animal_tid]), &META)
            .unwrap();

        // Dog <: Animal
        // ---------------------------
        // Variant<Animal> <: Variant<Dog>
        // Variant<Dog> </: Variant<Animal>

        assert!(typestore
            .check_subtype(&env, variant_animal, variant_dog, &ORIGIN, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(&env, variant_dog, variant_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );
    }

    #[test]
    fn invariant_variant_subtype() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let enum_tid = invariant_enum(&mut typestore);
        let variant_tid = typestore.get_enum_variant(enum_tid, 0, &META).unwrap();

        let variant_dog = typestore
            .type_instance(&env, variant_tid, Some(vec![dog_tid]), &META)
            .unwrap();

        let variant_animal = typestore
            .type_instance(&env, variant_tid, Some(vec![animal_tid]), &META)
            .unwrap();

        // Dog <: Animal
        // ---------------------------
        // Variant<Dog> </: Variant<Animal>
        // Variant<Animal> </: Variant<Dog>

        assert_eq!(
            typestore.check_subtype(&env, variant_dog, variant_animal, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &ORIGIN, &META
            ))
        );

        assert_eq!(
            typestore.check_subtype(&env, variant_animal, variant_dog, &ORIGIN, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &ORIGIN, &META
            ))
        );

        assert!(typestore
            .check_subtype(&env, variant_dog, variant_dog, &ORIGIN, &META)
            .is_ok());

        assert!(typestore
            .check_subtype(&env, variant_animal, variant_animal, &ORIGIN, &META)
            .is_ok());
    }

    #[test]
    fn same_variant_join() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let (_, some, _) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(&env, some, Some(vec![dog_tid]), &META)
            .unwrap();

        let some_animal = typestore
            .type_instance(&env, some, Some(vec![animal_tid]), &META)
            .unwrap();

        // Dog <: Animal
        //
        // join(
        //     Some<Dog>,
        //     Some<Animal>
        // )
        // = Some<Animal>

        assert_eq!(
            typestore.join_ty_ty(&env, some_dog, some_animal, &ORIGIN, &META),
            Ok(some_animal)
        );

        assert_eq!(
            typestore.join_ty_ty(&env, some_animal, some_dog, &ORIGIN, &META),
            Ok(some_animal)
        );
    }

    #[test]
    fn sibling_variants_join() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let (option, some, none) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(&env, some, Some(vec![dog_tid]), &META)
            .unwrap();

        let none_animal = typestore
            .type_instance(&env, none, Some(vec![animal_tid]), &META)
            .unwrap();

        let option_animal = typestore
            .type_instance(&env, option, Some(vec![animal_tid]), &META)
            .unwrap();

        // Dog <: Animal
        //
        // join(
        //     Some<Dog>,
        //     None<Animal>
        // )
        // = Option<Animal>

        assert_eq!(
            typestore.join_ty_ty(&env, some_dog, none_animal, &ORIGIN, &META),
            Ok(option_animal)
        );

        assert_eq!(
            typestore.join_ty_ty(&env, none_animal, some_dog, &ORIGIN, &META),
            Ok(option_animal)
        );
    }

    #[test]
    fn variant_parent_join() {
        let env = Environment::new();
        let mut typestore = TypeStore::new();
        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let (option, some, _) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(&env, some, Some(vec![dog_tid]), &META)
            .unwrap();

        let option_animal = typestore
            .type_instance(&env, option, Some(vec![animal_tid]), &META)
            .unwrap();

        // Dog <: Animal
        //
        // Some<Dog> <: Option<Animal>
        //
        // therefore:
        //
        // join(
        //     Some<Dog>,
        //     Option<Animal>
        // )
        // = Option<Animal>

        assert_eq!(
            typestore.join_ty_ty(&env, some_dog, option_animal, &ORIGIN, &META),
            Ok(option_animal)
        );

        assert_eq!(
            typestore.join_ty_ty(&env, option_animal, some_dog, &ORIGIN, &META),
            Ok(option_animal)
        );
    }
}
