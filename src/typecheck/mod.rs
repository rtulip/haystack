use std::{assert_eq, collections::HashMap, format, todo};

use crate::{
    error::HayError,
    lex::token::{Literal, Token},
    parser::{
        expr::{ParseExpr, ParseExprKind},
        statement::{EnumDefinition, FunctionDefinition, Statememt, StructDefinition},
        typ::{ParamatarizedIdent, ParseType, ParseTypePath},
    },
    typecheck::{
        environment::{Environment, Scheme},
        expr::{Expr, Var},
        func::Function,
        typ::{
            typ::{EnumDef, EnumType, FunctionType, Mutability, StructDef, Type},
            type_id::{EnumId, StructId, TypeId},
            type_var::TypeVar,
        },
        typestore::{
            inference::{InferenceError, InferenceErrorKind},
            TypeStore,
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

pub struct TypeCheck<'src> {
    typestore: TypeStore,
    env: Environment,
    symbols: SymbolTable<'src>,
    scope: Scope<'src>,
}

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
    variants: HashMap<(EnumId, usize), Token<'src>>,
    members: HashMap<(StructId, usize), Token<'src>>,
}

impl<'src> SymbolTable<'src> {
    pub fn new(typestore: &mut TypeStore) -> Self {
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
        ]);
        SymbolTable {
            types,
            vars: HashMap::new(),
            variants: HashMap::new(),
            members: HashMap::new(),
        }
    }

    pub fn insert_tid(&mut self, tid: TypeId, info: TypeInfo<'src>) -> Option<TypeInfo<'src>> {
        self.types.insert(tid, info)
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

    pub fn insert_var(&mut self, var: Var, info: VarInfo<'src>) -> Option<VarInfo<'src>> {
        self.vars.insert(var, info)
    }
}

struct ScopeLayer<'src> {
    types: HashMap<String, (TypeId, Option<Token<'src>>)>,
    vars: HashMap<&'src str, (Var, Token<'src>)>,
    variants: HashMap<(EnumId, &'src str), usize>,
    members: HashMap<(StructId, &'src str), usize>,
}

struct Scope<'src> {
    scopes: Vec<ScopeLayer<'src>>,
}

impl<'src> Scope<'src> {
    pub fn new(typestore: &mut TypeStore) -> Self {
        let types = HashMap::from([
            ("u8".to_owned(), (typestore.intern(Type::u8()), None)),
            ("u64".to_owned(), (typestore.intern(Type::u64()), None)),
            ("bool".to_owned(), (typestore.intern(Type::bool()), None)),
        ]);

        Scope {
            scopes: vec![ScopeLayer {
                types,
                vars: HashMap::new(),
                variants: HashMap::new(),
                members: HashMap::new(),
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

    fn get_member(&mut self, sid: StructId, name: &'src str) -> Option<usize> {
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

    fn new_scope(&mut self) {
        self.scopes.push(ScopeLayer {
            types: HashMap::new(),
            vars: HashMap::new(),
            variants: HashMap::new(),
            members: HashMap::new(),
        })
    }

    fn end_scope(&mut self) {
        assert!(self.scopes.len() > 1);
        self.scopes.pop();
    }
}

impl<'src> TypeCheck<'src> {
    fn insert_var(&mut self, token: &Token<'src>) -> Result<Var, HayError> {
        let var = self.env.fresh_var();
        if let Some((var, tok)) = self.scope.insert_var(token.lexeme, var, token.clone()) {
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

    fn insert_param_ty(&mut self, ty: &ParseType<'src>) -> Result<TypeVar, HayError> {
        let tyvar = self.typestore.fresh_type_var();
        let tid = self.typestore.intern(Type::TypeVar(tyvar));
        let token = ty.token();
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
        assert!(self
            .symbols
            .insert_tid(
                tid,
                TypeInfo {
                    name: token.lexeme.into(),
                    token: Some(token)
                }
            )
            .is_none());
        Ok(tyvar)
    }

    fn get_metatype_tid(&mut self, ident: &ParamatarizedIdent<'src>) -> Result<TypeId, HayError> {
        assert_eq!(ident.base.lexeme, "Type");

        if ident.params.len() != 1 {
            todo!("Wrong number of params for `Type`");
        }

        let inner_tid = self.get_tid(&ident.params[0])?;
        let tid = self
            .typestore
            .metatype(inner_tid, None, &ident.base)
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

        let args = if params.len() == 0 {
            None
        } else {
            Some(params)
        };

        let tid = self
            .typestore
            .type_instance(base_tid, args, &ident.base)
            .map_err(|e| self.map_inference_err(e))?;
        Ok(tid)
    }

    fn get_enum_variant(
        &mut self,
        enum_tid: TypeId,
        eid: EnumId,
        ident: &ParamatarizedIdent<'src>,
    ) -> Result<TypeId, HayError> {
        let variant_id = match self.scope.get_variant(eid, ident.base.lexeme) {
            Some(variant_id) => variant_id,
            None => todo!(),
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
            ParseType::Path(path) => {
                self.get_type_path_tid(path)

                // let Some((base_tid, _)) = self.scope.get_tid(&base.lexeme.into()) else {
                //     todo!("Unknown type {base}");
                // };

                // todo!()
            }
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
        }
    }

    fn lower_expr(&mut self, expr: &ParseExpr<'src>) -> Result<Expr<'src, Token<'src>>, HayError> {
        match &expr.kind {
            ParseExprKind::Ident(ident) => {
                let Some((var, _)) = self.scope.get_var(ident) else {
                    todo!("No var found for `{ident}`")
                };
                Ok(Expr::ident(var, expr.token.clone()))
            }
            ParseExprKind::Literal(literal) => match literal {
                Literal::Bool(b) => Ok(Expr::bool(*b, expr.token.clone())),
                Literal::Char(_) => todo!(),
                Literal::String(s) => Ok(Expr::string(s, expr.token.clone())),
                Literal::U8(_) => todo!(),
                Literal::U64(n) => Ok(Expr::u64(*n, expr.token.clone())),
            },
            ParseExprKind::Block(parse_exprs) => {
                let mut exprs = vec![];
                for expr in parse_exprs {
                    exprs.push(self.lower_expr(expr)?);
                }

                Ok(Expr::block(exprs, expr.token.clone()))
            }
            ParseExprKind::Type(ty) => {
                let tid = self.get_tid(ty)?;
                let tid = self
                    .typestore
                    .metatype(tid, None, &expr.token)
                    .map_err(|e| todo!("{e:?}"))?;
                Ok(Expr::typ(tid, expr.token.clone()))
            }
            ParseExprKind::Pack => Ok(Expr::pack(expr.token.clone())),
            ParseExprKind::Call => Ok(Expr::call(expr.token.clone())),
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
            ParseExprKind::TakeRef { mutable, ident } => {
                let Some((var, _)) = self.scope.get_var(*ident) else {
                    todo!("Unknown var `{ident}");
                };

                let mutability = if *mutable {
                    Mutability::Mutable
                } else {
                    Mutability::Const
                };

                Ok(Expr::take_ref(var, mutability, expr.token.clone()))
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
            Type::TypeVar(_) => todo!(),
            Type::RigidTypeVar(tyvar) => {
                let tid = self.typestore.intern(Type::TypeVar(tyvar));
                if let Some(info) = self.symbols.types.get(&tid) {
                    info.name.clone()
                } else {
                    todo!()
                }
            }
            Type::Builtin(_) => todo!(),
            Type::Function(func) => {
                let mut s = format!("fn{}", self.format_stack(&func.input, '(', ')'));
                if !func.output.is_empty() {
                    s = format!("{s} -> {}", self.format_stack(&func.output, '[', ']'));
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
            Type::Struct(_) => todo!(),
            Type::Enum(e) => self.format_enum(e.eid, e.args, None),
            Type::Tuple(tids) => self.format_stack(&tids, '(', ')'),
            Type::Variant(v) => self.format_enum(v.eid, v.args, Some(v.variant)),
            Type::Type(tid) => {
                format!("Type<{}>", self.format_tid(tid))
            }
        }
    }

    fn format_tid(&mut self, tid: TypeId) -> String {
        let tid = self.typestore.substitute_one(tid).1;

        if let Some(info) = self.symbols.types.get(&tid) {
            info.name.clone()
        } else {
            if let Some(origin) = self.typestore.tyvar_origin.get(&tid) {
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

    fn map_inference_err(&mut self, e: InferenceError<Token<'src>>) -> HayError {
        match e.kind {
            InferenceErrorKind::NotEnoughArgs { expected, stack } => {
                HayError::new("Too few arguments on the stack", e.meta.loc.clone())
                    .with_hint(format!(
                        "Expected: {}",
                        self.format_stack(&expected, '[', ']')
                    ))
                    .with_hint(format!("Found   : {}", self.format_stack(&stack, '[', ']')))
            }
            InferenceErrorKind::StacksDiffer { expected, found } => {
                HayError::new("Stacks Different", e.meta.loc.clone())
                    .with_hint(format!(
                        "Expected: {}",
                        self.format_stack(&expected, '[', ']')
                    ))
                    .with_hint(format!("Found   : {}", self.format_stack(&found, '[', ']')))
            }
            InferenceErrorKind::TypeParameterConstrained { .. } => todo!(),
            InferenceErrorKind::UnboundVar(..) => todo!(),
            InferenceErrorKind::TypesIncompatible { expected, found } => HayError::new(
                format!("Incompatible type for `{}`", e.meta.lexeme),
                e.meta.loc.clone(),
            )
            .with_hint(format!("Expected: {}", self.format_tid(expected)))
            .with_hint(format!("Found   : {}", self.format_tid(found))),
            InferenceErrorKind::MutableRefToFunction(..) => todo!(),
            InferenceErrorKind::PredicateNotMet { .. } => todo!(),
            InferenceErrorKind::ConflictingImpls { .. } => todo!(),
        }
    }

    fn bind_function_def(&mut self, func: &FunctionDefinition<'src>) -> Result<(), HayError> {
        // insert this function into the global scope
        let var = self.insert_var(&func.name)?;

        // create a new scope for types and identifiers
        self.scope.new_scope();
        {
            let mut params = vec![];
            for ty in &func.params {
                params.push(self.insert_param_ty(ty)?);
            }

            let mut binds = vec![];
            let mut input = vec![];
            for arg in &func.signature.input {
                let tid = self.get_tid(&arg.typ)?;
                input.push(tid);

                if let Some(ident) = &arg.ident {
                    let var = self.insert_var(ident)?;
                    binds.push(var);
                }
            }

            let mut output = vec![];
            for ty in &func.signature.output {
                let tid = self.get_tid(&ty)?;
                output.push(tid);
            }

            let tid = self.typestore.function(FunctionType {
                input: input.clone(),
                output: output.clone(),
            });
            let scheme = Scheme::new(params.clone(), tid, vec![]);
            assert!(self.env.bind_scheme(var, scheme).is_none());

            let mut expr = self.lower_expr(&func.expr)?;

            if binds.len() != 0 {
                expr = Expr::block(
                    [Expr::bind(binds, func.expr.token.clone()), expr],
                    func.expr.token.clone(),
                );
            }

            let f = Function::new(input, output, params, vec![], expr);
            let f = f
                .type_check(&mut self.typestore, &self.env)
                .map_err(|e| self.map_inference_err(e))?;
        }
        self.scope.end_scope();
        Ok(())
    }

    fn new() -> Self {
        let mut typestore = TypeStore::new();
        let symbols = SymbolTable::new(&mut typestore);
        let scope = Scope::new(&mut typestore);
        let env = Environment::new();

        Self {
            typestore,
            env,
            symbols,
            scope,
        }
    }

    fn bind_struct_def(&mut self, s: &StructDefinition<'src>) -> Result<(), HayError> {
        // Only have the vars known within the scope of the member definition.
        self.scope.new_scope();

        let vars = s
            .params
            .iter()
            .map(|ty| self.insert_param_ty(ty))
            .collect::<Result<Vec<_>, _>>()?;
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

        Ok(())
    }

    fn bind_enum_def(&mut self, e: &EnumDefinition<'src>) -> Result<(), HayError> {
        // Only have the vars known within the scope of the member definition.
        self.scope.new_scope();

        let vars = e
            .params
            .iter()
            .map(|ty| self.insert_param_ty(ty))
            .collect::<Result<Vec<_>, _>>()?;

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

        Ok(())
    }

    pub fn build_world(mut statements: Vec<Statememt<'src>>) -> Result<(), HayError> {
        let mut tc = TypeCheck::new();

        for statement in &statements {
            match statement {
                Statememt::StructDefinition(s) => tc.bind_struct_def(s)?,
                Statememt::EnumDefinition(e) => tc.bind_enum_def(e)?,
                Statememt::FunctionDefinition(func) => tc.bind_function_def(func)?,
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        typecheck::environment::{
            Coherence, Environment, Interface, InterfaceImpl, Scheme, TypePredicate,
        },
        typecheck::expr::{Expr, MatchCase, MatchExpr, MatchPattern, Var},
        typecheck::func::Function,
        typecheck::typ::typ::{EnumDef, FunctionType, Mutability, PointerType, StructDef, Type},
        typecheck::typ::type_id::TypeId,
        typecheck::typestore::inference::{InferenceError, InferenceErrorKind},
        typecheck::typestore::TypeStore,
    };

    const META: () = ();

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

        let f = dup.type_check(&mut typestore, &env);
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

        let f = bad_dup.type_check(&mut typestore, &env);
        assert_eq!(
            f,
            Err(InferenceError::type_param_constrained(
                typevar,
                typestore.intern(Type::u8()),
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
            output: vec![dup_typevar_tid, dup_typevar_tid],
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

        let f = test.type_check(&mut typestore, &env);
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
            output: vec![bool_tid],
        });
        let tid = typestore.intern(ty);

        // Map the identifier `t` to the TypeId for `T` in the environment
        let gt_scheme = Scheme::new([], tid, []);
        env.bind_scheme(gt, gt_scheme);

        // Manually create the body of the expression.
        let expr = Expr::block([Expr::u8(0, ()), Expr::u8(0, ()), Expr::ident(gt, ())], ());

        let stack = vec![];
        let target = vec![bool_tid];

        assert!(typestore
            .type_check(expr, &stack, &target, &mut env)
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
            output: vec![],
        });
        let tid = typestore.intern(ty);

        // Map the identifier `t` to the TypeId for `T` in the environment
        let foo_scheme = Scheme::new([], tid, []);
        env.bind_scheme(foo, foo_scheme);

        // Manually create the body of the expression.
        let expr = Expr::ident(foo, ());

        let stack = vec![];
        let target = vec![];

        assert_eq!(
            typestore.type_check(expr, &stack, &target, &mut env),
            Err(InferenceError::not_enough_args(vec![u8_tid], vec![], &META))
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

        assert!(swap.type_check(&mut typestore, &env).is_ok());
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

        match swap.type_check(&mut typestore, &env) {
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
            output: vec![],
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
            output: vec![foo_param_tid],
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
            test.type_check(&mut typestore, &env),
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
            output: vec![t_tid, t_tid],
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
        assert!(test.type_check(&mut typestore, &env).is_ok());
    }

    #[test]
    fn simple_mismatch() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let u8_tid = typestore.intern(Type::u8());
        let bool_tid = typestore.intern(Type::bool());
        let test = Function::new([], [u8_tid], [], [], Expr::bool(true, ()));

        assert_eq!(
            test.type_check(&mut typestore, &env),
            Err(InferenceError::types_incompatible(u8_tid, bool_tid, &META))
        );
    }

    #[test]
    fn simple_ref() {
        let mut typestore = TypeStore::new();
        let env = Environment::new();

        let x = Var(0);

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
                [Expr::bind(x, ()), Expr::take_ref(x, Mutability::Const, ())],
                (),
            ),
        );

        assert!(test.type_check(&mut typestore, &env).is_ok());
    }

    #[test]
    fn fn_pointer_call() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let dup = Var(0);
        let dup_tyvar = typestore.fresh_type_var();
        let dup_t_tid = typestore.intern(Type::TypeVar(dup_tyvar));
        let dup_tid = typestore.intern(Type::Function(FunctionType {
            input: vec![dup_t_tid],
            output: vec![dup_t_tid, dup_t_tid],
        }));

        let dup_scheme = Scheme::new([dup_tyvar], dup_tid, vec![]);
        env.bind_scheme(dup, dup_scheme);

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
        assert!(test.type_check(&mut typestore, &env).is_ok());
    }

    #[test]
    fn mutable_fn_pointer() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let f = Var(0);
        let func_tid = typestore.function(FunctionType {
            input: vec![],
            output: vec![],
        });
        let func_scheme = Scheme::new([], func_tid, []);
        env.bind_scheme(f, func_scheme);

        let test = Function::new(
            [],
            [func_tid],
            [],
            [],
            Expr::take_ref(f, Mutability::Mutable, ()),
        );

        assert_eq!(
            test.type_check(&mut typestore, &env),
            Err(InferenceError::mutable_ref_to_fn(f, &META))
        );
    }

    #[test]
    fn fn_pointer_bad_args() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let u8_tid = typestore.intern(Type::u8());
        let bool_tid = typestore.intern(Type::bool());
        let f = Var(0);
        let func_tid = typestore.function(FunctionType {
            input: vec![u8_tid],
            output: vec![],
        });

        let func_scheme = Scheme::new([], func_tid, []);
        env.bind_scheme(f, func_scheme);

        let test_not_enough_args = Function::new(
            [],
            [],
            [],
            [],
            Expr::block(
                [Expr::take_ref(f, Mutability::Const, ()), Expr::call(())],
                (),
            ),
        );

        assert_eq!(
            test_not_enough_args.type_check(&mut typestore, &env),
            Err(InferenceError::not_enough_args(vec![u8_tid], vec![], &META))
        );

        let test_bad_args = Function::new(
            [],
            [],
            [],
            [],
            Expr::block(
                [
                    Expr::bool(true, ()),
                    Expr::take_ref(f, Mutability::Const, ()),
                    Expr::call(()),
                ],
                (),
            ),
        );

        assert_eq!(
            test_bad_args.type_check(&mut typestore, &env),
            Err(InferenceError::types_incompatible(u8_tid, bool_tid, &META))
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
            output: vec![t_ptr_tid],
        });

        let foo_scheme = Scheme::new([t_typevar], foo_tid, []);
        env.bind_scheme(foo, foo_scheme);

        let test = Function::new([t_tid], [t_ptr_tid], [t_typevar], [], Expr::ident(foo, ()));

        assert!(test.type_check(&mut typestore, &env).is_ok());
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
            output: vec![],
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

        assert!(test1.type_check(&mut typestore, &env).is_ok());

        let identity = Var(1);
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let identity_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![t_tid],
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
        assert!(test2.type_check(&mut typestore, &env).is_ok());

        let test3 = Function::new(
            [u8_const_ptr_tid],
            [u8_mut_ptr_tid],
            [],
            [],
            Expr::block([], ()),
        );
        assert_eq!(
            test3.type_check(&mut typestore, &env),
            Err(InferenceError::types_incompatible(
                u8_mut_ptr_tid,
                u8_const_ptr_tid,
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
        let t_typ_tid = typestore.metatype(t_tid, None, &META).unwrap();
        let u8_tid = typestore.intern(Type::u8());
        let u8_t_tid = typestore.metatype(u8_tid, None, &META).unwrap();

        let sizeof = Var(0);
        let sizeof_tid = typestore.function(FunctionType {
            input: vec![t_typ_tid],
            output: vec![u8_tid],
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
        assert!(test.type_check(&mut typestore, &env).is_ok());
    }

    #[test]
    fn type_of() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let t_ty_tid = typestore.metatype(t_tid, None, &META).unwrap();

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
            output: vec![t_ty_tid],
        });

        let type_of_scheme = Scheme::new([t_tyvar], type_of_tid, []);

        env.bind_scheme(type_of, type_of_scheme);
        assert!(type_of_impl.type_check(&mut typestore, &env).is_ok());

        let u8_tid = typestore.intern(Type::u8());
        let u8_t_tid = typestore.metatype(u8_tid, None, &META).unwrap();

        let call_type_of = Function::new(
            [],
            [u8_t_tid],
            [],
            [],
            Expr::block([Expr::u8(0, ()), Expr::ident(type_of, ())], ()),
        );

        assert!(call_type_of.type_check(&mut typestore, &env).is_ok());
    }

    #[test]
    fn impl_not_found() {
        let mut typestore = TypeStore::new();
        let mut env = Environment::new();

        let t_tyvar = typestore.fresh_type_var();
        let foo = Interface {
            vars: vec![t_tyvar],
            coherence: Coherence::Unique,
        };

        let foo_cid = env.add_typeclass(foo);

        let bar = Var(0);
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let bar_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![],
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
            test.type_check(&mut typestore, &env),
            Err(InferenceError::predicate_not_met(
                foo_cid,
                vec![u8_tid],
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
                    predicates: vec![],
                },
                &META
            )
            .is_ok());

        let bar = Var(0);
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let bar_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![],
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
        assert!(test.type_check(&mut typestore, &env).is_ok());
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
                    predicates: vec![],
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
                    predicates: vec![],
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
            output: vec![into_tid],
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
        assert!(test.type_check(&mut typestore, &env).is_ok());

        let test = Function::new([u8_tid], [bool_ptr_tid], [], [], Expr::ident(cast, ()));
        assert_eq!(
            test.type_check(&mut typestore, &env),
            Err(InferenceError::predicate_not_met(
                cast_cid,
                vec![u8_tid, bool_ptr_tid],
                &META
            ))
        );

        let test = Function::new([u8_ptr_tid], [bool_tid], [], [], Expr::ident(cast, ()));
        assert_eq!(
            test.type_check(&mut typestore, &env),
            Err(InferenceError::predicate_not_met(
                cast_cid,
                vec![u8_ptr_tid, bool_tid],
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
            predicates: vec![],
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
            predicates: vec![TypePredicate {
                cid: print_cid,
                args: vec![t_tid],
            }],
        };
        assert!(env
            .add_impl(&mut typestore, print_cid, ptr_impl, &META)
            .is_ok());

        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let print_fn_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![],
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
        assert!(u8_print.type_check(&mut typestore, &env).is_ok());

        let bool_print = Function::new([bool_tid], [], [], [], Expr::ident(print, ()));
        assert_eq!(
            bool_print.type_check(&mut typestore, &env),
            Err(InferenceError::predicate_not_met(
                print_cid,
                vec![bool_tid],
                &META
            ))
        );

        let u8_ptr_print = Function::new([u8_ptr_tid], [], [], [], Expr::ident(print, ()));
        assert!(u8_ptr_print.type_check(&mut typestore, &env).is_ok());

        let bool_ptr_print = Function::new([bool_ptr_tid], [], [], [], Expr::ident(print, ()));
        assert_eq!(
            bool_ptr_print.type_check(&mut typestore, &env),
            Err(InferenceError::predicate_not_met(
                print_cid,
                vec![bool_ptr_tid],
                &META
            ))
        );

        let u8_ptr_ptr_tid = typestore.pointer(u8_ptr_tid, Mutability::Const);

        let u8_ptr_ptr_print = Function::new([u8_ptr_ptr_tid], [], [], [], Expr::ident(print, ()));

        assert!(u8_ptr_ptr_print.type_check(&mut typestore, &env).is_ok());

        let bool_ptr_ptr_tid = typestore.pointer(bool_ptr_tid, Mutability::Const);

        let bool_ptr_ptr_print =
            Function::new([bool_ptr_ptr_tid], [], [], [], Expr::ident(print, ()));

        assert_eq!(
            bool_ptr_ptr_print.type_check(&mut typestore, &env),
            Err(InferenceError::predicate_not_met(
                print_cid,
                vec![bool_ptr_ptr_tid],
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
            output: vec![],
        });

        let print_iface = Interface {
            vars: vec![t_tyvar],
            coherence: Coherence::Unique,
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
            predicates: vec![],
        };

        let print_impl_bool = InterfaceImpl {
            cid: print_cid,
            vars: vec![],
            args: vec![bool_tid],
            predicates: vec![],
        };

        assert!(env
            .add_impl(&mut typestore, print_cid, print_impl_u8, &META)
            .is_ok());

        let main = Function::new([u8_tid], [], [], [], Expr::ident(print, ()));
        assert!(main.type_check(&mut typestore, &env).is_ok());
        let main = Function::new([bool_tid], [], [], [], Expr::ident(print, ()));
        assert_eq!(
            main.type_check(&mut typestore, &env),
            Err(InferenceError::predicate_not_met(
                print_cid,
                vec![bool_tid],
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
        assert!(main.type_check(&mut typestore, &env).is_ok());

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
        assert!(println.type_check(&mut typestore, &env).is_ok());
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
            output: vec![],
        });

        let print_iface = Interface {
            vars: vec![t_tyvar],
            coherence: Coherence::Unique,
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
            predicates: vec![],
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
        assert!(println.type_check(&mut typestore, &env).is_ok());
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
        };

        let cast_cid = env.add_typeclass(cast_iface);

        let from_tyvar = typestore.fresh_type_var();
        let from_tid = typestore.intern(Type::TypeVar(from_tyvar));
        let into_tyvar = typestore.fresh_type_var();
        let into_tid = typestore.intern(Type::TypeVar(into_tyvar));
        let into_ty_tid = typestore.metatype(into_tid, None, &META).unwrap();

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
        assert!(cast.type_check(&mut typestore, &env).is_ok());
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
            .type_instance(pair_tid, Some(vec![bool_tid]), &META)
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
                    Expr::typ(typestore.metatype(pair_tid, None, &META).unwrap(), ()),
                    Expr::pack(()),
                ],
                (),
            ),
        );

        assert!(example.type_check(&mut typestore, &env).is_ok());

        let bad_pack = Function::new(
            [],
            [pair_bool_tid],
            [],
            [],
            Expr::block(
                [
                    Expr::u8(0, ()),
                    Expr::bool(true, ()),
                    Expr::typ(typestore.metatype(pair_tid, None, &META).unwrap(), ()),
                    Expr::pack(()),
                ],
                (),
            ),
        );

        let u8_tid = typestore.intern(Type::u8());
        assert_eq!(
            bad_pack.type_check(&mut typestore, &env),
            Err(InferenceError::types_incompatible(bool_tid, u8_tid, &META))
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
            .type_instance(option_none_tid, Some(vec![t_tid]), &META)
            .unwrap();
        let option_none_ty_tid = typestore.metatype(option_none_tid, None, &META).unwrap();

        let option_none = Function::new(
            [],
            [option_none_t_tid],
            [t_tyvar],
            [],
            Expr::block([Expr::typ(option_none_ty_tid, ()), Expr::pack(())], ()),
        );

        assert!(option_none.type_check(&mut typestore, &env).is_ok());
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
            .type_instance(option_tid, Some(vec![t_tid]), &META)
            .unwrap();

        let option_some_tid = typestore.get_enum_variant(option_tid, 0, &META).unwrap();
        let option_some_ty_tid = typestore.metatype(option_some_tid, None, &META).unwrap();

        let option_some = Function::new(
            [t_tid],
            [option_t_tid],
            [t_tyvar],
            [],
            Expr::block([Expr::typ(option_some_ty_tid, ()), Expr::pack(())], ()),
        );

        assert!(option_some.type_check(&mut typestore, &env).is_ok());
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
            .type_instance(option_tid, Some(vec![bool_tid]), &META)
            .unwrap();

        let generate_option = Var(0);
        let generate_option_tid = typestore.function(FunctionType {
            input: vec![],
            output: vec![option_t_tid],
        });
        let generate_option_scheme = Scheme::new([], generate_option_tid, []);
        env.bind_scheme(generate_option, generate_option_scheme);

        let option_some_tid = typestore.get_enum_variant(option_tid, 0, &META).unwrap();
        let option_some_t_tid = typestore
            .type_instance(option_some_tid, Some(vec![bool_tid]), &META)
            .unwrap();

        let take_option_some = Var(1);
        let take_option_some_tid = typestore.function(FunctionType {
            input: vec![option_some_t_tid],
            output: vec![],
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
            test.type_check(&mut typestore, &env),
            Err(InferenceError::types_incompatible(
                option_some_t_tid,
                option_t_tid,
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

        let test = Function::new(
            [str_tid],
            [u8_tid],
            [],
            [],
            Expr::block(
                [Expr::bind(Var(0), ()), Expr::member_access(Var(0), 0, ())],
                (),
            ),
        );

        assert!(test.type_check(&mut typestore, &env).is_ok());
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
            .type_instance(pair_tid, Some(vec![bool_tid]), &META)
            .unwrap();
        let test = Function::new(
            [pair_bool_tid],
            [bool_tid],
            [],
            [],
            Expr::block(
                [Expr::bind(Var(0), ()), Expr::member_access(Var(0), 0, ())],
                (),
            ),
        );
        assert!(test.type_check(&mut typestore, &env).is_ok());
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
                            Some(Var(0)),
                            Expr::block([Expr::bind(Var(1), ()), Expr::ident(Var(0), ())], ()),
                        ),
                        MatchCase::new(
                            MatchPattern::Variant(option_u8_none_tid),
                            None,
                            Expr::block([], ()),
                        ),
                    ],
                    None,
                ),
                (),
            ),
        );

        assert!(unwrap_or_default.type_check(&mut typestore, &env).is_ok());
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
            .type_instance(option_tid, Some(vec![u8_tid]), &META)
            .unwrap();

        let option_some_tid = typestore.get_enum_variant(option_tid, 0, &META).unwrap();
        let option_some_ty_tid = typestore.metatype(option_some_tid, None, &META).unwrap();
        let option_none_tid = typestore.get_enum_variant(option_tid, 1, &META).unwrap();
        let option_none_ty_tid = typestore.metatype(option_none_tid, None, &META).unwrap();

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
                            None,
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
                            None,
                            Expr::block([Expr::typ(option_none_ty_tid, ()), Expr::pack(())], ()),
                        ),
                    ],
                    None,
                ),
                (),
            ),
        );

        assert!(test.type_check(&mut typestore, &env).is_ok());
    }

    #[test]
    fn match_supertyping_pointers() {
        // enum Foo { u8: Bar u8: Baz }
        // fn test(Foo) {
        //     match {
        //         Foo::Bar as [v] &mut v
        //         Foo::Baz as [v] &v
        //     }
        //     as [_]
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
                            Some(Var(0)),
                            Expr::take_ref(Var(0), Mutability::Mutable, ()),
                        ),
                        MatchCase::new(
                            MatchPattern::Variant(foo_baz_tid),
                            Some(Var(0)),
                            Expr::take_ref(Var(0), Mutability::Const, ()),
                        ),
                    ],
                    None,
                ),
                (),
            ),
        );
        assert!(test.type_check(&mut typestore, &env).is_ok());

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
                            Some(Var(0)),
                            Expr::take_ref(Var(0), Mutability::Const, ()),
                        ),
                        MatchCase::new(
                            MatchPattern::Variant(foo_baz_tid),
                            Some(Var(0)),
                            Expr::take_ref(Var(0), Mutability::Mutable, ()),
                        ),
                    ],
                    None,
                ),
                (),
            ),
        );
        assert!(test.type_check(&mut typestore, &env).is_ok());
    }

    fn option_def(typestore: &mut TypeStore) -> (TypeId, TypeId, TypeId) {
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
            .type_instance(option, Some(vec![u8_tid]), &META)
            .unwrap();
        let option_some_option_u8 = typestore
            .type_instance(some, Some(vec![option_u8]), &META)
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
                            None,
                            Expr::block(
                                [
                                    Expr::typ(
                                        typestore
                                            .metatype(none, Some(vec![u8_tid]), &META)
                                            .unwrap(),
                                        (),
                                    ),
                                    Expr::pack(()),
                                    Expr::typ(typestore.metatype(some, None, &META).unwrap(), ()),
                                    Expr::pack(()),
                                ],
                                (),
                            ),
                        ),
                        MatchCase::new(
                            MatchPattern::Variant(foo_baz),
                            None,
                            Expr::block(
                                [
                                    Expr::u8(0, ()),
                                    Expr::typ(typestore.metatype(some, None, &META).unwrap(), ()),
                                    Expr::pack(()),
                                    Expr::typ(typestore.metatype(some, None, &META).unwrap(), ()),
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

        assert!(test.type_check(&mut typestore, &env).is_ok());
    }
}

#[cfg(test)]
mod variance_tests {

    use crate::{
        typecheck::environment::Variance,
        typecheck::typ::typ::{EnumDef, FunctionType, Mutability, StructDef, Type},
        typecheck::typ::type_id::TypeId,
        typecheck::typestore::inference::InferenceError,
        typecheck::typestore::TypeStore,
    };

    const META: () = ();

    fn animal_def(typestore: &mut TypeStore) -> (TypeId, TypeId) {
        let animal = EnumDef::new(&typestore, vec![], vec![None], &META).unwrap();
        let (animal_tid, _) = typestore.add_enum(animal);
        let dog_tid = typestore.get_enum_variant(animal_tid, 0, &META).unwrap();
        (animal_tid, dog_tid)
    }

    fn option_def(typestore: &mut TypeStore) -> (TypeId, TypeId, TypeId) {
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

        // Dog <: Animal
        // -------------
        // &Dog <: &Animal

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let dog_ptr = typestore.pointer(dog_tid, Mutability::Const);

        assert!(typestore.check_subtype(dog_ptr, animal_ptr, &META).is_ok());
    }

    #[test]
    fn mut_ptr_const_ptr_subtype() {
        let mut typestore = TypeStore::new();

        // Dog <: Animal
        // -------------
        // &mut Dog <: &Animal

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let dog_mut_ptr = typestore.pointer(dog_tid, Mutability::Mutable);

        assert!(typestore
            .check_subtype(dog_mut_ptr, animal_ptr, &META)
            .is_ok());
    }

    #[test]
    fn const_ptr_mut_ptr_subtype() {
        let mut typestore = TypeStore::new();

        // Dog <: Animal
        // -------------
        // &Dog </: &mut Animal

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_mut_ptr = typestore.pointer(animal_tid, Mutability::Mutable);
        let dog_ptr = typestore.pointer(dog_tid, Mutability::Const);

        assert_eq!(
            typestore.check_subtype(dog_ptr, animal_mut_ptr, &META),
            Err(InferenceError::types_incompatible(
                animal_mut_ptr,
                dog_ptr,
                &META
            ))
        );
    }

    #[test]
    fn mut_ptr_mut_ptr_subtype() {
        let mut typestore = TypeStore::new();

        // Dog <: Animal
        // -------------
        // &mut Dog </: &mut Animal

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_mut_ptr = typestore.pointer(animal_tid, Mutability::Mutable);
        let dog_mut_ptr = typestore.pointer(dog_tid, Mutability::Mutable);

        assert_eq!(
            typestore.check_subtype(dog_mut_ptr, animal_mut_ptr, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
    }

    #[test]
    fn mut_ptr_mut_ptr_equal_t_subtype() {
        let mut typestore = TypeStore::new();

        // Animal <: Animal
        // -------------
        // &mut Animal <: &mut Animal

        let animal = EnumDef::new(&typestore, vec![], vec![None], &META).unwrap();
        let (animal_tid, _) = typestore.add_enum(animal);
        let animal_mut_ptr = typestore.pointer(animal_tid, Mutability::Mutable);

        assert!(typestore
            .check_subtype(animal_mut_ptr, animal_mut_ptr, &META)
            .is_ok());
    }

    #[test]
    fn const_ptr_nested_subtype() {
        let mut typestore = TypeStore::new();

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
            .check_subtype(dog_ptr_mut_ptr, animal_ptr_ptr, &META)
            .is_ok());
    }

    #[test]
    fn mut_ptr_nested_subtype() {
        let mut typestore = TypeStore::new();

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
            typestore.check_subtype(dog_ptr_mut_ptr, animal_ptr_mut_ptr, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
    }

    #[test]
    fn const_sub_ptr_const_super_ptr_join() {
        let mut typestore = TypeStore::new();

        // Animal >: Dog
        // -------------
        // &Animal >: &Dog

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let dog_ptr = typestore.pointer(dog_tid, Mutability::Const);

        assert_eq!(
            typestore.join_ty_ty(dog_ptr, animal_ptr, &META),
            Ok(animal_ptr)
        );
        assert_eq!(
            typestore.join_ty_ty(animal_ptr, dog_ptr, &META),
            Ok(animal_ptr)
        );
    }

    #[test]
    fn mut_sub_ptr_const_super_ptr_join() {
        let mut typestore = TypeStore::new();

        // Animal >: Dog
        // -------------
        // &Animal >: &mut Dog

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let dog_mut_ptr = typestore.pointer(dog_tid, Mutability::Mutable);

        assert_eq!(
            typestore.join_ty_ty(dog_mut_ptr, animal_ptr, &META),
            Ok(animal_ptr)
        );
        assert_eq!(
            typestore.join_ty_ty(animal_ptr, dog_mut_ptr, &META),
            Ok(animal_ptr)
        );
    }

    #[test]
    fn const_sub_ptr_mut_super_ptr_join() {
        let mut typestore = TypeStore::new();

        // Animal >: Dog
        // &Animal >: &Dog
        // -------------
        // &Animal >: &mut Dog

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let animal_mut_ptr = typestore.pointer(animal_tid, Mutability::Mutable);
        let dog_ptr = typestore.pointer(dog_tid, Mutability::Const);

        assert_eq!(
            typestore.join_ty_ty(dog_ptr, animal_mut_ptr, &META),
            Ok(animal_ptr)
        );
        assert_eq!(
            typestore.join_ty_ty(animal_mut_ptr, dog_ptr, &META),
            Ok(animal_ptr)
        );
    }

    #[test]
    fn mut_sub_ptr_mut_super_ptr_join() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let animal_mut_ptr = typestore.pointer(animal_tid, Mutability::Mutable);
        let dog_mut_ptr = typestore.pointer(dog_tid, Mutability::Mutable);

        assert_eq!(
            typestore.join_ty_ty(dog_mut_ptr, animal_mut_ptr, &META),
            Ok(animal_ptr)
        );
        assert_eq!(
            typestore.join_ty_ty(animal_mut_ptr, dog_mut_ptr, &META),
            Ok(animal_ptr)
        );
    }

    #[test]
    fn mut_t_ptr_mut_t_ptr_join() {
        let mut typestore = TypeStore::new();

        let animal = EnumDef::new(&typestore, vec![], vec![None], &META).unwrap();
        let (animal_tid, _) = typestore.add_enum(animal);
        let animal_mut_ptr = typestore.pointer(animal_tid, Mutability::Mutable);

        assert_eq!(
            typestore.join_ty_ty(animal_mut_ptr, animal_mut_ptr, &META),
            Ok(animal_mut_ptr)
        );
    }

    #[test]
    fn mut_t_ptr_const_t_ptr_join() {
        let mut typestore = TypeStore::new();

        let animal = EnumDef::new(&typestore, vec![], vec![None], &META).unwrap();
        let (animal_tid, _) = typestore.add_enum(animal);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let animal_mut_ptr = typestore.pointer(animal_tid, Mutability::Mutable);

        assert_eq!(
            typestore.join_ty_ty(animal_mut_ptr, animal_ptr, &META),
            Ok(animal_ptr)
        );
    }

    #[test]
    fn const_sub_ptr_const_super_ptr_nested_join() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let animal_ptr_ptr = typestore.pointer(animal_ptr, Mutability::Const);
        let dog_mut_ptr = typestore.pointer(dog_tid, Mutability::Mutable);
        let dog_mut_ptr_ptr = typestore.pointer(dog_mut_ptr, Mutability::Const);

        assert_eq!(
            typestore.join_ty_ty(dog_mut_ptr_ptr, animal_ptr_ptr, &META),
            Ok(animal_ptr_ptr)
        );
        assert_eq!(
            typestore.join_ty_ty(animal_ptr_ptr, dog_mut_ptr_ptr, &META),
            Ok(animal_ptr_ptr)
        );
    }

    #[test]
    fn mut_sub_ptr_mut_super_ptr_nested_join() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let animal_ptr = typestore.pointer(animal_tid, Mutability::Const);
        let animal_ptr_ptr = typestore.pointer(animal_ptr, Mutability::Const);
        let animal_ptr_mut_ptr = typestore.pointer(animal_ptr, Mutability::Mutable);
        let dog_ptr = typestore.pointer(dog_tid, Mutability::Const);
        let dog_ptr_mut_ptr = typestore.pointer(dog_ptr, Mutability::Mutable);

        assert_eq!(
            typestore.join_ty_ty(animal_ptr_mut_ptr, dog_ptr_mut_ptr, &META),
            Ok(animal_ptr_ptr)
        );
        assert_eq!(
            typestore.join_ty_ty(dog_ptr_mut_ptr, animal_ptr_mut_ptr, &META),
            Ok(animal_ptr_ptr)
        );
    }

    fn covariant_struct(typestore: &mut TypeStore) -> TypeId {
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let def = StructDef::new(&typestore, vec![t_tyvar], vec![t_tid], &META).unwrap();
        assert!(def.variances.iter().all(|v| *v == Variance::Covariant));
        typestore.add_struct(def).0
    }

    fn invariant_struct(typestore: &mut TypeStore) -> TypeId {
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let t_ptr_mut_tid = typestore.pointer(t_tid, Mutability::Mutable);
        let def = StructDef::new(&typestore, vec![t_tyvar], vec![t_ptr_mut_tid], &META).unwrap();
        assert!(def.variances.iter().all(|v| *v == Variance::Invariant));
        typestore.add_struct(def).0
    }

    fn contravariant_struct(typestore: &mut TypeStore) -> TypeId {
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![],
        });

        let def = StructDef::new(&typestore, vec![t_tyvar], vec![f_tid], &META).unwrap();
        assert!(def.variances.iter().all(|v| *v == Variance::Contravariant));
        typestore.add_struct(def).0
    }

    #[test]
    fn covariant_struct_subtype() {
        let mut typestore = TypeStore::new();

        // Dog <: Animal
        // -------------
        // Box<Dog> <: Box<Animal>

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let covariant_struct = covariant_struct(&mut typestore);
        let covariant_struct_animal = typestore
            .type_instance(covariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_struct_dog = typestore
            .type_instance(covariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(covariant_struct_dog, covariant_struct_animal, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(covariant_struct_animal, covariant_struct_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
    }

    #[test]
    fn invariant_struct_subtype() {
        let mut typestore = TypeStore::new();

        // Dog <: Animal
        // -------------
        // Invariant<Animal> <:  Invariant<Animal>
        // Invariant<Dog>    </: Invariant<Animal>
        // Invariant<Animal> </: Invariant<Dog>
        // Invariant<Dog>    <:  Invariant<Dog>

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let invariant_struct = invariant_struct(&mut typestore);
        let invariant_struct_animal = typestore
            .type_instance(invariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let invariant_struct_dog = typestore
            .type_instance(invariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(invariant_struct_animal, invariant_struct_animal, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(invariant_struct_animal, invariant_struct_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(invariant_struct_dog, invariant_struct_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(invariant_struct_animal, invariant_struct_animal, &META)
            .is_ok());
    }

    #[test]
    fn contravariant_struct_subtype() {
        let mut typestore = TypeStore::new();

        // Dog <: Animal
        // -------------
        // Contravariant<Animal> <:  Contravariant<Animal>
        // Contravariant<Dog>    </: Contravariant<Animal>
        // Contravariant<Animal> <: Contravariant<Dog>
        // Contravariant<Dog>    <:  Contravariant<Dog>

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let contravariant_struct = contravariant_struct(&mut typestore);
        let contravariant_struct_animal = typestore
            .type_instance(contravariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_struct_dog = typestore
            .type_instance(contravariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(
                contravariant_struct_animal,
                contravariant_struct_animal,
                &META
            )
            .is_ok());
        assert_eq!(
            typestore.check_subtype(contravariant_struct_dog, contravariant_struct_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(contravariant_struct_animal, contravariant_struct_dog, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(contravariant_struct_dog, contravariant_struct_dog, &META)
            .is_ok());
    }

    #[test]
    fn nested_covariant_covaraint_struct_subtype() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        // -------------
        // Covariant<Dog> <: Covariant<Animal>
        // -----------------------------------
        // Covariant<Covariant<Dog>>    <:  Covariant<Covariant<Animal>>
        // Covariant<Covariant<Animal>> </: Covariant<Covariant<Dog>>

        let covariant_struct = covariant_struct(&mut typestore);
        let covariant_dog = typestore
            .type_instance(covariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let covariant_covariant_dog = typestore
            .type_instance(covariant_struct, Some(vec![covariant_dog]), &META)
            .unwrap();
        let covariant_animal = typestore
            .type_instance(covariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_covariant_animal = typestore
            .type_instance(covariant_struct, Some(vec![covariant_animal]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(covariant_covariant_dog, covariant_covariant_animal, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(covariant_covariant_animal, covariant_covariant_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
    }

    #[test]
    fn nested_covariant_contravariant_struct_subtype() {
        let mut typestore = TypeStore::new();

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
            .type_instance(contravariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let covariant_contravariant_dog = typestore
            .type_instance(covariant_struct, Some(vec![contravariant_dog]), &META)
            .unwrap();
        let contravariant_animal = typestore
            .type_instance(contravariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_contravariant_animal = typestore
            .type_instance(covariant_struct, Some(vec![contravariant_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.check_subtype(
                covariant_contravariant_dog,
                covariant_contravariant_animal,
                &META
            ),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(
                covariant_contravariant_animal,
                covariant_contravariant_dog,
                &META
            )
            .is_ok());
    }

    #[test]
    fn nested_contravariant_covariant_struct_subtype() {
        let mut typestore = TypeStore::new();

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
            .type_instance(covariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let contravariant_covariant_dog = typestore
            .type_instance(contravariant_struct, Some(vec![covariant_dog]), &META)
            .unwrap();
        let covariant_animal = typestore
            .type_instance(covariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_covariant_animal = typestore
            .type_instance(contravariant_struct, Some(vec![covariant_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.check_subtype(
                contravariant_covariant_dog,
                contravariant_covariant_animal,
                &META
            ),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(
                contravariant_covariant_animal,
                contravariant_covariant_dog,
                &META
            )
            .is_ok());
    }

    #[test]
    fn nested_contravariant_contravariant_struct_subtype() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        // -------------
        // Contravariant<Animal> <: Contravariant<Dog>
        // -----------------------------------
        // Contravariant<Contravariant<Dog>>    <:  Contravariant<Contravariant<Animal>>
        // Contravariant<Contravariant<Animal>> </: Contravariant<Contravariant<Dog>>

        let contravariant_struct = contravariant_struct(&mut typestore);
        let contravariant_dog = typestore
            .type_instance(contravariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let contravariant_contravariant_dog = typestore
            .type_instance(contravariant_struct, Some(vec![contravariant_dog]), &META)
            .unwrap();
        let contravariant_animal = typestore
            .type_instance(contravariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_contravariant_animal = typestore
            .type_instance(
                contravariant_struct,
                Some(vec![contravariant_animal]),
                &META,
            )
            .unwrap();

        assert!(typestore
            .check_subtype(
                contravariant_contravariant_dog,
                contravariant_contravariant_animal,
                &META
            )
            .is_ok());
        assert_eq!(
            typestore.check_subtype(
                contravariant_contravariant_animal,
                contravariant_contravariant_dog,
                &META
            ),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
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

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let contravariant_struct = contravariant_struct(&mut typestore);
        let covariant_struct = covariant_struct(&mut typestore);
        let invariant_struct = invariant_struct(&mut typestore);

        let invariant_dog = typestore
            .type_instance(invariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let covariant_dog = typestore
            .type_instance(covariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let contravariant_dog = typestore
            .type_instance(contravariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let invariant_animal = typestore
            .type_instance(invariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_animal = typestore
            .type_instance(covariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_animal = typestore
            .type_instance(contravariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();

        let co_inv_dog = typestore
            .type_instance(covariant_struct, Some(vec![invariant_dog]), &META)
            .unwrap();
        let co_inv_animal = typestore
            .type_instance(covariant_struct, Some(vec![invariant_animal]), &META)
            .unwrap();
        let contra_inv_dog = typestore
            .type_instance(contravariant_struct, Some(vec![invariant_dog]), &META)
            .unwrap();
        let contra_inv_animal = typestore
            .type_instance(contravariant_struct, Some(vec![invariant_animal]), &META)
            .unwrap();
        let inv_co_dog = typestore
            .type_instance(invariant_struct, Some(vec![covariant_dog]), &META)
            .unwrap();
        let inv_co_animal = typestore
            .type_instance(invariant_struct, Some(vec![covariant_animal]), &META)
            .unwrap();
        let inv_contra_dog = typestore
            .type_instance(invariant_struct, Some(vec![contravariant_dog]), &META)
            .unwrap();
        let inv_contra_animal = typestore
            .type_instance(invariant_struct, Some(vec![contravariant_animal]), &META)
            .unwrap();
        let inv_inv_dog = typestore
            .type_instance(invariant_struct, Some(vec![invariant_dog]), &META)
            .unwrap();
        let inv_inv_animal = typestore
            .type_instance(invariant_struct, Some(vec![invariant_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.check_subtype(co_inv_dog, co_inv_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(co_inv_animal, co_inv_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(co_inv_dog, co_inv_dog, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(co_inv_animal, co_inv_animal, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(contra_inv_dog, contra_inv_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(contra_inv_animal, contra_inv_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(contra_inv_dog, contra_inv_dog, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(contra_inv_animal, contra_inv_animal, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(inv_co_dog, inv_co_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(inv_co_animal, inv_co_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(inv_co_dog, inv_co_dog, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(inv_co_animal, inv_co_animal, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(inv_contra_dog, inv_contra_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(inv_contra_animal, inv_contra_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(inv_contra_dog, inv_contra_dog, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(inv_contra_animal, inv_contra_animal, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(inv_inv_dog, inv_inv_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(inv_inv_animal, inv_inv_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(inv_inv_dog, inv_inv_dog, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(inv_inv_animal, inv_inv_animal, &META)
            .is_ok());
    }

    #[test]
    fn mixed_variance_struct_subtype() {
        let mut typestore = TypeStore::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        // Contravariant on `B`
        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![],
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
            .type_instance(mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();
        let mixed_animal_dog = typestore
            .type_instance(mixed, Some(vec![animal_tid, dog_tid]), &META)
            .unwrap();
        let mixed_dog_dog = typestore
            .type_instance(mixed, Some(vec![dog_tid, dog_tid]), &META)
            .unwrap();
        let mixed_animal_animal = typestore
            .type_instance(mixed, Some(vec![animal_tid, animal_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(mixed_dog_animal, mixed_animal_dog, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(mixed_animal_dog, mixed_dog_animal, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(mixed_dog_dog, mixed_animal_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(mixed_dog_animal, mixed_animal_animal, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(mixed_dog_animal, mixed_dog_dog, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(mixed_dog_dog, mixed_dog_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(mixed_dog_animal, mixed_dog_dog, &META)
            .is_ok());
    }

    #[test]
    fn different_structs_subtype() {
        let mut typestore = TypeStore::new();
        let a = covariant_struct(&mut typestore);
        let b = contravariant_struct(&mut typestore);

        assert_eq!(
            typestore.check_subtype(a, b, &META),
            Err(InferenceError::types_incompatible(b, a, &META))
        )
    }

    #[test]
    fn covariant_struct_join() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let covariant_struct = covariant_struct(&mut typestore);
        let box_animal = typestore
            .type_instance(covariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let box_dog = typestore
            .type_instance(covariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(box_animal, box_dog, &META),
            Ok(box_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(box_dog, box_animal, &META),
            Ok(box_animal)
        );
    }

    #[test]
    fn invariant_struct_join() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let invariant_struct = invariant_struct(&mut typestore);
        let inv_animal = typestore
            .type_instance(invariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let inv_dog = typestore
            .type_instance(invariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(inv_animal, inv_animal, &META),
            Ok(inv_animal)
        );
        assert_eq!(typestore.join_ty_ty(inv_dog, inv_dog, &META), Ok(inv_dog));
        assert_eq!(
            typestore.join_ty_ty(inv_animal, inv_dog, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert_eq!(
            typestore.join_ty_ty(inv_dog, inv_animal, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
    }

    #[test]
    fn contravariant_struct_join() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let contravariant_struct = contravariant_struct(&mut typestore);
        let contra_animal = typestore
            .type_instance(contravariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();
        let contra_dog = typestore
            .type_instance(contravariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(contra_animal, contra_animal, &META),
            Ok(contra_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(contra_dog, contra_dog, &META),
            Ok(contra_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(contra_animal, contra_dog, &META),
            Ok(contra_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(contra_dog, contra_animal, &META),
            Ok(contra_dog)
        );
    }

    #[test]
    fn nested_covariant_covariant_struct_join() {
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

        let covariant_struct = covariant_struct(&mut typestore);

        let co_dog = typestore
            .type_instance(covariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let co_animal = typestore
            .type_instance(covariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();

        let co_co_dog = typestore
            .type_instance(covariant_struct, Some(vec![co_dog]), &META)
            .unwrap();
        let co_co_animal = typestore
            .type_instance(covariant_struct, Some(vec![co_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(co_co_dog, co_co_animal, &META),
            Ok(co_co_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(co_co_animal, co_co_dog, &META),
            Ok(co_co_animal)
        );
    }

    #[test]
    fn nested_covariant_contravariant_struct_join() {
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

        let covariant_struct = covariant_struct(&mut typestore);
        let contravariant_struct = contravariant_struct(&mut typestore);

        let contra_dog = typestore
            .type_instance(contravariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let contra_animal = typestore
            .type_instance(contravariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();

        let co_contra_dog = typestore
            .type_instance(covariant_struct, Some(vec![contra_dog]), &META)
            .unwrap();
        let co_contra_animal = typestore
            .type_instance(covariant_struct, Some(vec![contra_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(co_contra_dog, co_contra_animal, &META),
            Ok(co_contra_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(co_contra_animal, co_contra_dog, &META),
            Ok(co_contra_dog)
        );
    }

    #[test]
    fn nested_contravariant_covariant_struct_join() {
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

        let covariant_struct = covariant_struct(&mut typestore);
        let contravariant_struct = contravariant_struct(&mut typestore);

        let co_dog = typestore
            .type_instance(covariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let co_animal = typestore
            .type_instance(covariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();

        let contra_co_dog = typestore
            .type_instance(contravariant_struct, Some(vec![co_dog]), &META)
            .unwrap();
        let contra_co_animal = typestore
            .type_instance(contravariant_struct, Some(vec![co_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(contra_co_dog, contra_co_animal, &META),
            Ok(contra_co_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(contra_co_animal, contra_co_dog, &META),
            Ok(contra_co_dog)
        );
    }

    #[test]
    fn nested_contravariant_contravariant_struct_join() {
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

        let contravariant_struct = contravariant_struct(&mut typestore);

        let contra_dog = typestore
            .type_instance(contravariant_struct, Some(vec![dog_tid]), &META)
            .unwrap();
        let contra_animal = typestore
            .type_instance(contravariant_struct, Some(vec![animal_tid]), &META)
            .unwrap();

        let contra_contra_dog = typestore
            .type_instance(contravariant_struct, Some(vec![contra_dog]), &META)
            .unwrap();
        let contra_contra_animal = typestore
            .type_instance(contravariant_struct, Some(vec![contra_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(contra_contra_dog, contra_contra_animal, &META),
            Ok(contra_contra_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(contra_contra_animal, contra_contra_dog, &META),
            Ok(contra_contra_animal)
        );
    }

    #[test]
    fn mixed_variance_struct_join() {
        let mut typestore = TypeStore::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        // A is covariant, B is contravariant.
        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![],
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
            .type_instance(mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();

        let mixed_animal_dog = typestore
            .type_instance(mixed, Some(vec![animal_tid, dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(mixed_dog_animal, mixed_animal_dog, &META),
            Ok(mixed_animal_dog)
        );

        assert_eq!(
            typestore.join_ty_ty(mixed_animal_dog, mixed_dog_animal, &META),
            Ok(mixed_animal_dog)
        );
    }

    #[test]
    fn mixed_variance_struct_join_both_arguments() {
        let mut typestore = TypeStore::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![],
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
            .type_instance(mixed, Some(vec![dog_tid, dog_tid]), &META)
            .unwrap();

        let mixed_animal_animal = typestore
            .type_instance(mixed, Some(vec![animal_tid, animal_tid]), &META)
            .unwrap();

        let mixed_animal_dog = typestore
            .type_instance(mixed, Some(vec![animal_tid, dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(mixed_dog_dog, mixed_animal_animal, &META),
            Ok(mixed_animal_dog)
        );

        assert_eq!(
            typestore.join_ty_ty(mixed_animal_animal, mixed_dog_dog, &META),
            Ok(mixed_animal_dog)
        );
    }

    #[test]
    fn mixed_variance_struct_join_contravariant_argument() {
        let mut typestore = TypeStore::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![],
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
            .type_instance(mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();

        let mixed_dog_dog = typestore
            .type_instance(mixed, Some(vec![dog_tid, dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(mixed_dog_animal, mixed_dog_dog, &META),
            Ok(mixed_dog_dog)
        );

        assert_eq!(
            typestore.join_ty_ty(mixed_dog_dog, mixed_dog_animal, &META),
            Ok(mixed_dog_dog)
        );
    }

    #[test]
    fn mixed_variance_struct_join_covariant_argument() {
        let mut typestore = TypeStore::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![],
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
            .type_instance(mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();

        let mixed_animal_animal = typestore
            .type_instance(mixed, Some(vec![animal_tid, animal_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(mixed_dog_animal, mixed_animal_animal, &META),
            Ok(mixed_animal_animal)
        );

        assert_eq!(
            typestore.join_ty_ty(mixed_animal_animal, mixed_dog_animal, &META),
            Ok(mixed_animal_animal)
        );
    }

    #[test]
    fn different_structs_join() {
        let mut typestore = TypeStore::new();

        let a = covariant_struct(&mut typestore);
        let b = contravariant_struct(&mut typestore);

        assert_eq!(
            typestore.join_ty_ty(a, b, &META),
            Err(InferenceError::types_incompatible(a, b, &META))
        )
    }

    fn covariant_enum(typestore: &mut TypeStore) -> TypeId {
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let def = EnumDef::new(&typestore, vec![t_tyvar], vec![Some(t_tid)], &META).unwrap();
        assert!(def.variances.iter().all(|v| *v == Variance::Covariant));
        typestore.add_enum(def).0
    }

    fn invariant_enum(typestore: &mut TypeStore) -> TypeId {
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));
        let t_ptr_mut_tid = typestore.pointer(t_tid, Mutability::Mutable);
        let def =
            EnumDef::new(&typestore, vec![t_tyvar], vec![Some(t_ptr_mut_tid)], &META).unwrap();
        assert!(def.variances.iter().all(|v| *v == Variance::Invariant));
        typestore.add_enum(def).0
    }

    fn contravariant_enum(typestore: &mut TypeStore) -> TypeId {
        let t_tyvar = typestore.fresh_type_var();
        let t_tid = typestore.intern(Type::TypeVar(t_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![t_tid],
            output: vec![],
        });

        let def = EnumDef::new(&typestore, vec![t_tyvar], vec![Some(f_tid)], &META).unwrap();
        assert!(def.variances.iter().all(|v| *v == Variance::Contravariant));
        typestore.add_enum(def).0
    }

    #[test]
    fn covariant_enum_subtype() {
        let mut typestore = TypeStore::new();

        // Dog <: Animal
        // -------------
        // Box<Dog> <: Box<Animal>

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let covariant_enum = covariant_enum(&mut typestore);
        let covariant_enum_animal = typestore
            .type_instance(covariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_enum_dog = typestore
            .type_instance(covariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(covariant_enum_dog, covariant_enum_animal, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(covariant_enum_animal, covariant_enum_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
    }

    #[test]
    fn invariant_enum_subtype() {
        let mut typestore = TypeStore::new();

        // Dog <: Animal
        // -------------
        // Invariant<Animal> <:  Invariant<Animal>
        // Invariant<Dog>    </: Invariant<Animal>
        // Invariant<Animal> </: Invariant<Dog>
        // Invariant<Dog>    <:  Invariant<Dog>

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let invariant_enum = invariant_enum(&mut typestore);
        let invariant_enum_animal = typestore
            .type_instance(invariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let invariant_enum_dog = typestore
            .type_instance(invariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(invariant_enum_animal, invariant_enum_animal, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(invariant_enum_animal, invariant_enum_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(invariant_enum_dog, invariant_enum_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(invariant_enum_animal, invariant_enum_animal, &META)
            .is_ok());
    }

    #[test]
    fn contravariant_enum_subtype() {
        let mut typestore = TypeStore::new();

        // Dog <: Animal
        // -------------
        // Contravariant<Animal> <:  Contravariant<Animal>
        // Contravariant<Dog>    </: Contravariant<Animal>
        // Contravariant<Animal> <: Contravariant<Dog>
        // Contravariant<Dog>    <:  Contravariant<Dog>

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let contravariant_enum = contravariant_enum(&mut typestore);
        let contravariant_enum_animal = typestore
            .type_instance(contravariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_enum_dog = typestore
            .type_instance(contravariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(contravariant_enum_animal, contravariant_enum_animal, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(contravariant_enum_dog, contravariant_enum_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(contravariant_enum_animal, contravariant_enum_dog, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(contravariant_enum_dog, contravariant_enum_dog, &META)
            .is_ok());
    }

    #[test]
    fn nested_covariant_covaraint_enum_subtype() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        // -------------
        // Covariant<Dog> <: Covariant<Animal>
        // -----------------------------------
        // Covariant<Covariant<Dog>>    <:  Covariant<Covariant<Animal>>
        // Covariant<Covariant<Animal>> </: Covariant<Covariant<Dog>>

        let covariant_enum = covariant_enum(&mut typestore);
        let covariant_dog = typestore
            .type_instance(covariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let covariant_covariant_dog = typestore
            .type_instance(covariant_enum, Some(vec![covariant_dog]), &META)
            .unwrap();
        let covariant_animal = typestore
            .type_instance(covariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_covariant_animal = typestore
            .type_instance(covariant_enum, Some(vec![covariant_animal]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(covariant_covariant_dog, covariant_covariant_animal, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(covariant_covariant_animal, covariant_covariant_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
    }

    #[test]
    fn nested_covariant_contravariant_enum_subtype() {
        let mut typestore = TypeStore::new();

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
            .type_instance(contravariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let covariant_contravariant_dog = typestore
            .type_instance(covariant_enum, Some(vec![contravariant_dog]), &META)
            .unwrap();
        let contravariant_animal = typestore
            .type_instance(contravariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_contravariant_animal = typestore
            .type_instance(covariant_enum, Some(vec![contravariant_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.check_subtype(
                covariant_contravariant_dog,
                covariant_contravariant_animal,
                &META
            ),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(
                covariant_contravariant_animal,
                covariant_contravariant_dog,
                &META
            )
            .is_ok());
    }

    #[test]
    fn nested_contravariant_covariant_enum_subtype() {
        let mut typestore = TypeStore::new();

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
            .type_instance(covariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let contravariant_covariant_dog = typestore
            .type_instance(contravariant_enum, Some(vec![covariant_dog]), &META)
            .unwrap();
        let covariant_animal = typestore
            .type_instance(covariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_covariant_animal = typestore
            .type_instance(contravariant_enum, Some(vec![covariant_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.check_subtype(
                contravariant_covariant_dog,
                contravariant_covariant_animal,
                &META
            ),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(
                contravariant_covariant_animal,
                contravariant_covariant_dog,
                &META
            )
            .is_ok());
    }

    #[test]
    fn nested_contravariant_contravariant_enum_subtype() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        // Dog <: Animal
        // -------------
        // Contravariant<Animal> <: Contravariant<Dog>
        // -----------------------------------
        // Contravariant<Contravariant<Dog>>    <:  Contravariant<Contravariant<Animal>>
        // Contravariant<Contravariant<Animal>> </: Contravariant<Contravariant<Dog>>

        let contravariant_enum = contravariant_enum(&mut typestore);
        let contravariant_dog = typestore
            .type_instance(contravariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let contravariant_contravariant_dog = typestore
            .type_instance(contravariant_enum, Some(vec![contravariant_dog]), &META)
            .unwrap();
        let contravariant_animal = typestore
            .type_instance(contravariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_contravariant_animal = typestore
            .type_instance(contravariant_enum, Some(vec![contravariant_animal]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(
                contravariant_contravariant_dog,
                contravariant_contravariant_animal,
                &META
            )
            .is_ok());
        assert_eq!(
            typestore.check_subtype(
                contravariant_contravariant_animal,
                contravariant_contravariant_dog,
                &META
            ),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
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

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let contravariant_enum = contravariant_enum(&mut typestore);
        let covariant_enum = covariant_enum(&mut typestore);
        let invariant_enum = invariant_enum(&mut typestore);

        let invariant_dog = typestore
            .type_instance(invariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let covariant_dog = typestore
            .type_instance(covariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let contravariant_dog = typestore
            .type_instance(contravariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let invariant_animal = typestore
            .type_instance(invariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let covariant_animal = typestore
            .type_instance(covariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let contravariant_animal = typestore
            .type_instance(contravariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();

        let co_inv_dog = typestore
            .type_instance(covariant_enum, Some(vec![invariant_dog]), &META)
            .unwrap();
        let co_inv_animal = typestore
            .type_instance(covariant_enum, Some(vec![invariant_animal]), &META)
            .unwrap();
        let contra_inv_dog = typestore
            .type_instance(contravariant_enum, Some(vec![invariant_dog]), &META)
            .unwrap();
        let contra_inv_animal = typestore
            .type_instance(contravariant_enum, Some(vec![invariant_animal]), &META)
            .unwrap();
        let inv_co_dog = typestore
            .type_instance(invariant_enum, Some(vec![covariant_dog]), &META)
            .unwrap();
        let inv_co_animal = typestore
            .type_instance(invariant_enum, Some(vec![covariant_animal]), &META)
            .unwrap();
        let inv_contra_dog = typestore
            .type_instance(invariant_enum, Some(vec![contravariant_dog]), &META)
            .unwrap();
        let inv_contra_animal = typestore
            .type_instance(invariant_enum, Some(vec![contravariant_animal]), &META)
            .unwrap();
        let inv_inv_dog = typestore
            .type_instance(invariant_enum, Some(vec![invariant_dog]), &META)
            .unwrap();
        let inv_inv_animal = typestore
            .type_instance(invariant_enum, Some(vec![invariant_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.check_subtype(co_inv_dog, co_inv_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(co_inv_animal, co_inv_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(co_inv_dog, co_inv_dog, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(co_inv_animal, co_inv_animal, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(contra_inv_dog, contra_inv_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(contra_inv_animal, contra_inv_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(contra_inv_dog, contra_inv_dog, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(contra_inv_animal, contra_inv_animal, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(inv_co_dog, inv_co_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(inv_co_animal, inv_co_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(inv_co_dog, inv_co_dog, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(inv_co_animal, inv_co_animal, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(inv_contra_dog, inv_contra_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(inv_contra_animal, inv_contra_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(inv_contra_dog, inv_contra_dog, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(inv_contra_animal, inv_contra_animal, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(inv_inv_dog, inv_inv_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(inv_inv_animal, inv_inv_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(inv_inv_dog, inv_inv_dog, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(inv_inv_animal, inv_inv_animal, &META)
            .is_ok());
    }

    #[test]
    fn mixed_variance_enum_subtype() {
        let mut typestore = TypeStore::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        // Contravariant on `B`
        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![],
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
            .type_instance(mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();
        let mixed_animal_dog = typestore
            .type_instance(mixed, Some(vec![animal_tid, dog_tid]), &META)
            .unwrap();
        let mixed_dog_dog = typestore
            .type_instance(mixed, Some(vec![dog_tid, dog_tid]), &META)
            .unwrap();
        let mixed_animal_animal = typestore
            .type_instance(mixed, Some(vec![animal_tid, animal_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(mixed_dog_animal, mixed_animal_dog, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(mixed_animal_dog, mixed_dog_animal, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(mixed_dog_dog, mixed_animal_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(mixed_dog_animal, mixed_animal_animal, &META)
            .is_ok());
        assert!(typestore
            .check_subtype(mixed_dog_animal, mixed_dog_dog, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(mixed_dog_dog, mixed_dog_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert!(typestore
            .check_subtype(mixed_dog_animal, mixed_dog_dog, &META)
            .is_ok());
    }

    #[test]
    fn different_enums_subtype() {
        let mut typestore = TypeStore::new();
        let a = covariant_enum(&mut typestore);
        let b = contravariant_enum(&mut typestore);

        assert_eq!(
            typestore.check_subtype(a, b, &META),
            Err(InferenceError::types_incompatible(b, a, &META))
        )
    }

    #[test]
    fn covariant_enum_join() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let covariant_enum = covariant_enum(&mut typestore);
        let box_animal = typestore
            .type_instance(covariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let box_dog = typestore
            .type_instance(covariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(box_animal, box_dog, &META),
            Ok(box_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(box_dog, box_animal, &META),
            Ok(box_animal)
        );
    }

    #[test]
    fn invariant_enum_join() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let invariant_enum = invariant_enum(&mut typestore);
        let inv_animal = typestore
            .type_instance(invariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let inv_dog = typestore
            .type_instance(invariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(inv_animal, inv_animal, &META),
            Ok(inv_animal)
        );
        assert_eq!(typestore.join_ty_ty(inv_dog, inv_dog, &META), Ok(inv_dog));
        assert_eq!(
            typestore.join_ty_ty(inv_animal, inv_dog, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
        assert_eq!(
            typestore.join_ty_ty(inv_dog, inv_animal, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
    }

    #[test]
    fn contravariant_enum_join() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let contravariant_enum = contravariant_enum(&mut typestore);
        let contra_animal = typestore
            .type_instance(contravariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();
        let contra_dog = typestore
            .type_instance(contravariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(contra_animal, contra_animal, &META),
            Ok(contra_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(contra_dog, contra_dog, &META),
            Ok(contra_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(contra_animal, contra_dog, &META),
            Ok(contra_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(contra_dog, contra_animal, &META),
            Ok(contra_dog)
        );
    }

    #[test]
    fn nested_covariant_covariant_enum_join() {
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
            .type_instance(covariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let co_animal = typestore
            .type_instance(covariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();

        let co_co_dog = typestore
            .type_instance(covariant_enum, Some(vec![co_dog]), &META)
            .unwrap();
        let co_co_animal = typestore
            .type_instance(covariant_enum, Some(vec![co_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(co_co_dog, co_co_animal, &META),
            Ok(co_co_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(co_co_animal, co_co_dog, &META),
            Ok(co_co_animal)
        );
    }

    #[test]
    fn nested_covariant_contravariant_enum_join() {
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
            .type_instance(contravariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let contra_animal = typestore
            .type_instance(contravariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();

        let co_contra_dog = typestore
            .type_instance(covariant_enum, Some(vec![contra_dog]), &META)
            .unwrap();
        let co_contra_animal = typestore
            .type_instance(covariant_enum, Some(vec![contra_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(co_contra_dog, co_contra_animal, &META),
            Ok(co_contra_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(co_contra_animal, co_contra_dog, &META),
            Ok(co_contra_dog)
        );
    }

    #[test]
    fn nested_contravariant_covariant_enum_join() {
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
            .type_instance(covariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let co_animal = typestore
            .type_instance(covariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();

        let contra_co_dog = typestore
            .type_instance(contravariant_enum, Some(vec![co_dog]), &META)
            .unwrap();
        let contra_co_animal = typestore
            .type_instance(contravariant_enum, Some(vec![co_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(contra_co_dog, contra_co_animal, &META),
            Ok(contra_co_dog)
        );
        assert_eq!(
            typestore.join_ty_ty(contra_co_animal, contra_co_dog, &META),
            Ok(contra_co_dog)
        );
    }

    #[test]
    fn nested_contravariant_contravariant_enum_join() {
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
            .type_instance(contravariant_enum, Some(vec![dog_tid]), &META)
            .unwrap();
        let contra_animal = typestore
            .type_instance(contravariant_enum, Some(vec![animal_tid]), &META)
            .unwrap();

        let contra_contra_dog = typestore
            .type_instance(contravariant_enum, Some(vec![contra_dog]), &META)
            .unwrap();
        let contra_contra_animal = typestore
            .type_instance(contravariant_enum, Some(vec![contra_animal]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(contra_contra_dog, contra_contra_animal, &META),
            Ok(contra_contra_animal)
        );
        assert_eq!(
            typestore.join_ty_ty(contra_contra_animal, contra_contra_dog, &META),
            Ok(contra_contra_animal)
        );
    }

    #[test]
    fn mixed_variance_enum_join() {
        let mut typestore = TypeStore::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        // A is covariant, B is contravariant.
        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![],
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
            .type_instance(mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();

        let mixed_animal_dog = typestore
            .type_instance(mixed, Some(vec![animal_tid, dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(mixed_dog_animal, mixed_animal_dog, &META),
            Ok(mixed_animal_dog)
        );

        assert_eq!(
            typestore.join_ty_ty(mixed_animal_dog, mixed_dog_animal, &META),
            Ok(mixed_animal_dog)
        );
    }

    #[test]
    fn mixed_variance_enum_join_both_arguments() {
        let mut typestore = TypeStore::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![],
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
            .type_instance(mixed, Some(vec![dog_tid, dog_tid]), &META)
            .unwrap();

        let mixed_animal_animal = typestore
            .type_instance(mixed, Some(vec![animal_tid, animal_tid]), &META)
            .unwrap();

        let mixed_animal_dog = typestore
            .type_instance(mixed, Some(vec![animal_tid, dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(mixed_dog_dog, mixed_animal_animal, &META),
            Ok(mixed_animal_dog)
        );

        assert_eq!(
            typestore.join_ty_ty(mixed_animal_animal, mixed_dog_dog, &META),
            Ok(mixed_animal_dog)
        );
    }

    #[test]
    fn mixed_variance_enum_join_contravariant_argument() {
        let mut typestore = TypeStore::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![],
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
            .type_instance(mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();

        let mixed_dog_dog = typestore
            .type_instance(mixed, Some(vec![dog_tid, dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(mixed_dog_animal, mixed_dog_dog, &META),
            Ok(mixed_dog_dog)
        );

        assert_eq!(
            typestore.join_ty_ty(mixed_dog_dog, mixed_dog_animal, &META),
            Ok(mixed_dog_dog)
        );
    }

    #[test]
    fn mixed_variance_enum_join_covariant_argument() {
        let mut typestore = TypeStore::new();

        let a_tyvar = typestore.fresh_type_var();
        let b_tyvar = typestore.fresh_type_var();
        let a_tid = typestore.intern(Type::TypeVar(a_tyvar));
        let b_tid = typestore.intern(Type::TypeVar(b_tyvar));

        let f_tid = typestore.function(FunctionType {
            input: vec![b_tid],
            output: vec![],
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
            .type_instance(mixed, Some(vec![dog_tid, animal_tid]), &META)
            .unwrap();

        let mixed_animal_animal = typestore
            .type_instance(mixed, Some(vec![animal_tid, animal_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.join_ty_ty(mixed_dog_animal, mixed_animal_animal, &META),
            Ok(mixed_animal_animal)
        );

        assert_eq!(
            typestore.join_ty_ty(mixed_animal_animal, mixed_dog_animal, &META),
            Ok(mixed_animal_animal)
        );
    }

    #[test]
    fn different_enums_join() {
        let mut typestore = TypeStore::new();

        let a = covariant_enum(&mut typestore);
        let b = contravariant_enum(&mut typestore);

        assert_eq!(
            typestore.join_ty_ty(a, b, &META),
            Err(InferenceError::types_incompatible(a, b, &META))
        )
    }

    #[test]
    fn same_variants_same_args_subtype() {
        let mut typestore = TypeStore::new();
        let (_, dog_tid) = animal_def(&mut typestore);
        let (_, some, _) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(some, Some(vec![dog_tid]), &META)
            .unwrap();
        assert!(typestore.check_subtype(some_dog, some_dog, &META).is_ok());
    }

    #[test]
    fn same_variants_subtype_args_subtype() {
        let mut typestore = TypeStore::new();
        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let (_, some, _) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(some, Some(vec![dog_tid]), &META)
            .unwrap();
        let some_animal = typestore
            .type_instance(some, Some(vec![animal_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(some_dog, some_animal, &META)
            .is_ok());
        assert_eq!(
            typestore.check_subtype(some_animal, some_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
    }

    #[test]
    fn different_variants_same_args_subtype() {
        let mut typestore = TypeStore::new();
        let (_, dog_tid) = animal_def(&mut typestore);
        let (_, some, none) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(some, Some(vec![dog_tid]), &META)
            .unwrap();
        let none_dog = typestore
            .type_instance(none, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.check_subtype(some_dog, none_dog, &META),
            Err(InferenceError::types_incompatible(
                none_dog, some_dog, &META
            ))
        );
        assert_eq!(
            typestore.check_subtype(none_dog, some_dog, &META),
            Err(InferenceError::types_incompatible(
                some_dog, none_dog, &META
            ))
        );
    }

    #[test]
    fn variant_parent_same_args_subtype() {
        let mut typestore = TypeStore::new();
        let (_, dog_tid) = animal_def(&mut typestore);
        let (option, some, _) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(some, Some(vec![dog_tid]), &META)
            .unwrap();
        let option_dog = typestore
            .type_instance(option, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore.check_subtype(some_dog, option_dog, &META).is_ok());
        assert_eq!(
            typestore.check_subtype(option_dog, some_dog, &META),
            Err(InferenceError::types_incompatible(
                some_dog, option_dog, &META
            ))
        );
    }

    #[test]
    fn variant_parent_subtype_args_subtype() {
        let mut typestore = TypeStore::new();
        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let (option, some, _) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(some, Some(vec![dog_tid]), &META)
            .unwrap();

        let some_animal = typestore
            .type_instance(some, Some(vec![animal_tid]), &META)
            .unwrap();

        let option_animal = typestore
            .type_instance(option, Some(vec![animal_tid]), &META)
            .unwrap();

        let option_dog = typestore
            .type_instance(option, Some(vec![dog_tid]), &META)
            .unwrap();

        assert!(typestore
            .check_subtype(some_dog, option_animal, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(option_animal, some_dog, &META),
            Err(InferenceError::types_incompatible(
                some_dog,
                option_animal,
                &META
            ))
        );

        assert_eq!(
            typestore.check_subtype(some_animal, option_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
    }

    #[test]
    fn enum_variants_mismatch_subtype() {
        let mut typestore = TypeStore::new();
        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let (_, some, _) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(some, Some(vec![dog_tid]), &META)
            .unwrap();

        assert_eq!(
            typestore.check_subtype(animal_tid, some_dog, &META),
            Err(InferenceError::types_incompatible(
                some_dog, animal_tid, &META
            ))
        )
    }

    #[test]
    fn covariant_variant_subtype() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let enum_tid = covariant_enum(&mut typestore);
        let variant_tid = typestore.get_enum_variant(enum_tid, 0, &META).unwrap();

        let variant_dog = typestore
            .type_instance(variant_tid, Some(vec![dog_tid]), &META)
            .unwrap();

        let variant_animal = typestore
            .type_instance(variant_tid, Some(vec![animal_tid]), &META)
            .unwrap();

        // Dog <: Animal
        // ---------------------------
        // Variant<Dog> <: Variant<Animal>

        assert!(typestore
            .check_subtype(variant_dog, variant_animal, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(variant_animal, variant_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );
    }

    #[test]
    fn contravariant_variant_subtype() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let enum_tid = contravariant_enum(&mut typestore);
        let variant_tid = typestore.get_enum_variant(enum_tid, 0, &META).unwrap();

        let variant_dog = typestore
            .type_instance(variant_tid, Some(vec![dog_tid]), &META)
            .unwrap();

        let variant_animal = typestore
            .type_instance(variant_tid, Some(vec![animal_tid]), &META)
            .unwrap();

        // Dog <: Animal
        // ---------------------------
        // Variant<Animal> <: Variant<Dog>
        // Variant<Dog> </: Variant<Animal>

        assert!(typestore
            .check_subtype(variant_animal, variant_dog, &META)
            .is_ok());

        assert_eq!(
            typestore.check_subtype(variant_dog, variant_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );
    }

    #[test]
    fn invariant_variant_subtype() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);

        let enum_tid = invariant_enum(&mut typestore);
        let variant_tid = typestore.get_enum_variant(enum_tid, 0, &META).unwrap();

        let variant_dog = typestore
            .type_instance(variant_tid, Some(vec![dog_tid]), &META)
            .unwrap();

        let variant_animal = typestore
            .type_instance(variant_tid, Some(vec![animal_tid]), &META)
            .unwrap();

        // Dog <: Animal
        // ---------------------------
        // Variant<Dog> </: Variant<Animal>
        // Variant<Animal> </: Variant<Dog>

        assert_eq!(
            typestore.check_subtype(variant_dog, variant_animal, &META),
            Err(InferenceError::types_incompatible(
                animal_tid, dog_tid, &META
            ))
        );

        assert_eq!(
            typestore.check_subtype(variant_animal, variant_dog, &META),
            Err(InferenceError::types_incompatible(
                dog_tid, animal_tid, &META
            ))
        );

        assert!(typestore
            .check_subtype(variant_dog, variant_dog, &META)
            .is_ok());

        assert!(typestore
            .check_subtype(variant_animal, variant_animal, &META)
            .is_ok());
    }

    #[test]
    fn same_variant_join() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let (_, some, _) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(some, Some(vec![dog_tid]), &META)
            .unwrap();

        let some_animal = typestore
            .type_instance(some, Some(vec![animal_tid]), &META)
            .unwrap();

        // Dog <: Animal
        //
        // join(
        //     Some<Dog>,
        //     Some<Animal>
        // )
        // = Some<Animal>

        assert_eq!(
            typestore.join_ty_ty(some_dog, some_animal, &META),
            Ok(some_animal)
        );

        assert_eq!(
            typestore.join_ty_ty(some_animal, some_dog, &META),
            Ok(some_animal)
        );
    }

    #[test]
    fn sibling_variants_join() {
        let mut typestore = TypeStore::new();

        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let (option, some, none) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(some, Some(vec![dog_tid]), &META)
            .unwrap();

        let none_animal = typestore
            .type_instance(none, Some(vec![animal_tid]), &META)
            .unwrap();

        let option_animal = typestore
            .type_instance(option, Some(vec![animal_tid]), &META)
            .unwrap();

        // Dog <: Animal
        //
        // join(
        //     Some<Dog>,
        //     None<Animal>
        // )
        // = Option<Animal>

        assert_eq!(
            typestore.join_ty_ty(some_dog, none_animal, &META),
            Ok(option_animal)
        );

        assert_eq!(
            typestore.join_ty_ty(none_animal, some_dog, &META),
            Ok(option_animal)
        );
    }

    #[test]
    fn variant_parent_join() {
        let mut typestore = TypeStore::new();
        let (animal_tid, dog_tid) = animal_def(&mut typestore);
        let (option, some, _) = option_def(&mut typestore);

        let some_dog = typestore
            .type_instance(some, Some(vec![dog_tid]), &META)
            .unwrap();

        let option_animal = typestore
            .type_instance(option, Some(vec![animal_tid]), &META)
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
            typestore.join_ty_ty(some_dog, option_animal, &META),
            Ok(option_animal)
        );

        assert_eq!(
            typestore.join_ty_ty(option_animal, some_dog, &META),
            Ok(option_animal)
        );
    }
}
