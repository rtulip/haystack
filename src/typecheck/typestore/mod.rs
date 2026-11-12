use std::{
    collections::{HashMap, HashSet},
    f32::consts::E,
    fmt::Debug,
    format,
    panic::Location,
    todo, unreachable, write,
};

use crate::typecheck::{
    constraint::{Constraint, ConstraintType},
    environment::{
        Coherence, Environment, InterfaceImpl, QualifiedInterfaceImpl, QualifiedType, Scheme,
        TypeClassId, TypePredicate, Variance,
    },
    expr::{Expr, ExprKind, MatchCase, MatchExpr, MatchPattern, Var},
    typ::{
        typ::{
            BuiltinType, EnumDef, EnumType, FunctionType, Mutability, PointerType, Projection,
            StructDef, StructType, Type, VariantType,
        },
        type_id::{EnumId, StructId, TypeId},
        type_var::TypeVar,
    },
    typestore::inference::{InferenceError, InferenceErrorKind},
    union_find::{UnifiactionError, UnificationTable},
};

use super::constraint::ConstraintOrigin;

pub mod inference;

pub type Stack = Vec<TypeId>;

pub trait MemberLookup {
    fn get_member_idx(&self, sid: StructId, member: &str) -> Option<usize>;
}

/// Typestore
///
/// This is a container that's responsible for mapping a TypeId into a type. This will mean that types
/// can be cheaper to work with, and not require deeply recursive definitions.
///
/// The type_lookup is the mapping from TypeId to the underlying type
///
/// Builtin types have to be searchable, so there's also a mapping from `BuiltinType` to `TypeId`.
pub struct TypeStore<M> {
    structs: Vec<StructDef>,
    enums: Vec<EnumDef>,
    types: Vec<Type>,
    interned: HashMap<Type, TypeId>,

    lookup: UnificationTable<TypeVar, TypeId>,
    pub tyvar_origin: HashMap<TypeId, (TypeId, M)>,
    str_tid: Option<TypeId>,
}

impl<M> TypeStore<M>
where
    M: Debug + Clone + PartialEq + Eq,
{
    /// Create a new typestore.
    pub fn new() -> Self {
        let builtins = [
            Type::Builtin(BuiltinType::U8),
            Type::Builtin(BuiltinType::U64),
            Type::Builtin(BuiltinType::Bool),
        ];

        TypeStore {
            structs: vec![],
            enums: vec![],
            types: Vec::from(builtins.clone()),
            interned: HashMap::from_iter(
                builtins
                    .into_iter()
                    .enumerate()
                    .map(|(idx, ty)| (ty, TypeId(idx))),
            ),
            lookup: UnificationTable::new(),
            tyvar_origin: HashMap::new(),
            str_tid: None,
        }
    }

    pub fn set_str_tid(&mut self, tid: TypeId) {
        assert!(self.str_tid == None);
        self.str_tid = Some(tid);
    }

    pub fn get_struct_def(&self, sid: StructId) -> &StructDef {
        &self.structs[sid.0]
    }

    pub fn get_enum_def(&self, eid: EnumId) -> &EnumDef {
        &self.enums[eid.0]
    }

    pub fn get_enum_variants(&mut self, eid: EnumId, args: Vec<TypeId>) -> Vec<Option<TypeId>> {
        let def = self.get_enum_def(eid).clone();
        assert!(def.vars.len() == args.len());
        let subs = HashMap::from_iter(def.vars.into_iter().zip(args.into_iter()));

        def.variants
            .into_iter()
            .map(|tid| {
                if let Some(tid) = tid {
                    Some(self.replace(tid, &subs))
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_struct_members(&mut self, s: StructType) -> Vec<TypeId> {
        let def = self.get_struct_def(s.sid).clone();
        assert!(def.vars.len() == s.args.len());
        let subs = HashMap::from_iter(def.vars.into_iter().zip(s.args.into_iter()));

        def.members
            .into_iter()
            .map(|tid| self.replace(tid, &subs))
            .collect()
    }

    pub fn add_struct(&mut self, def: StructDef) -> (TypeId, StructId) {
        let sid = StructId(self.structs.len());
        let s = StructType {
            sid,
            args: def
                .vars
                .iter()
                .map(|tid| self.intern(Type::TypeVar(*tid)))
                .collect(),
        };
        let tid = self.intern(Type::Struct(s));

        self.structs.push(def);
        (tid, sid)
    }

    pub fn add_enum(&mut self, def: EnumDef) -> (TypeId, EnumId) {
        let eid = EnumId(self.enums.len());
        let e = EnumType {
            eid,
            args: def
                .vars
                .iter()
                .map(|tid| self.intern(Type::TypeVar(*tid)))
                .collect(),
        };
        let tid = self.intern(Type::Enum(e));

        for i in 0..def.variants.len() {
            let variant_ty = VariantType {
                eid,
                args: def
                    .vars
                    .iter()
                    .map(|tid| self.intern(Type::TypeVar(*tid)))
                    .collect(),
                variant: i,
            };
            self.intern(Type::Variant(variant_ty));
        }

        self.enums.push(def);
        (tid, eid)
    }

    pub fn intern(&mut self, ty: Type) -> TypeId {
        if let Some(id) = self.interned.get(&ty) {
            return *id;
        }

        let id = TypeId(self.types.len());
        self.types.push(ty.clone());
        assert!(self.interned.insert(ty, id).is_none());
        id
    }

    pub fn get(&self, tid: TypeId) -> Option<&Type> {
        return self.types.get(tid.0);
    }

    /// Query a TypeId to see if it's a function.
    pub fn get_func(&self, tid: &TypeId) -> Option<&FunctionType> {
        if let Some(Type::Function(ref func)) = self.types.get(tid.0) {
            Some(func)
        } else {
            None
        }
    }

    pub fn pointer(&mut self, tid: TypeId, mutability: Mutability) -> TypeId {
        self.intern(Type::Pointer(PointerType { tid, mutability }))
    }

    pub fn function(&mut self, func: FunctionType) -> TypeId {
        self.intern(Type::Function(func))
    }

    pub fn tuple(&mut self, tids: Vec<TypeId>) -> TypeId {
        match tids.len() {
            1 => tids[0],
            _ => self.intern(Type::Tuple(tids)),
        }
    }

    fn pretty_print<W: std::fmt::Write>(
        &self,
        tid: TypeId,
        w: &mut W,
    ) -> Result<(), std::fmt::Error> {
        let t = &self.types[tid.0];
        match t {
            Type::Never => write!(w, "!"),
            Type::TypeVar(type_var) => write!(w, "TV({})", type_var.0),
            Type::RigidTypeVar(type_var) => write!(w, "RV({})", type_var.0),
            Type::Builtin(builtin_type) => match builtin_type {
                BuiltinType::U8 => write!(w, "u8"),
                BuiltinType::U64 => write!(w, "u64"),
                BuiltinType::Bool => write!(w, "bool"),
                BuiltinType::CVarArgs => write!(w, "..."),
            },
            Type::Function(function_type) => {
                let mut s = String::from("fn (");
                for tid in &function_type.input {
                    s += " ";
                    self.pretty_print(*tid, &mut s)?;
                }
                s += " ) -> [";

                for tid in &function_type.output {
                    s += " ";
                    self.pretty_print(*tid, &mut s)?;
                }

                s += " ]";

                write!(w, "{s}")
            }
            Type::Pointer(pointer_type) => match pointer_type.mutability {
                Mutability::Mutable => write!(w, "&mut {}", self.fmt(pointer_type.tid)),
                Mutability::Const => write!(w, "&{}", self.fmt(pointer_type.tid)),
            },
            Type::Type(tid) => {
                write!(w, "Type({})", self.fmt(*tid))
            }
            Type::Struct(s) => {
                write!(w, "struct({})< {} >", s.sid.0, self.fmt_many(&s.args))
            }
            Type::Enum(e) => write!(w, "enum({})< {} >", e.eid.0, self.fmt_many(&e.args)),
            Type::Variant(v) => write!(
                w,
                "enum({})< {} >::{}",
                v.eid.0,
                self.fmt_many(&v.args),
                v.variant,
            ),
            Type::Tuple(tids) => {
                let mut s = String::from("(");
                for tid in tids {
                    s += " ";
                    self.pretty_print(*tid, &mut s)?;
                }
                s += " )";
                write!(w, "{s}")
            }
            Type::Projection(p) => {
                write!(
                    w,
                    "Interface({})< {} >::{}",
                    p.cid.0,
                    self.fmt_many(&p.args),
                    p.idx
                )
            }
        }
    }

    pub fn fmt(&self, tid: TypeId) -> String {
        let mut s = String::new();
        self.pretty_print(tid, &mut s).unwrap();
        s
    }

    pub fn fmt_many<'a, I>(&self, tids: I) -> String
    where
        I: IntoIterator<Item = &'a TypeId>,
    {
        let tids: Vec<_> = tids.into_iter().map(|tid| self.fmt(*tid)).collect();
        format!("{tids:?}")
    }

    pub fn fmt_constraint(&self, constraint: &Constraint<M>) -> String {
        match &constraint.constraint {
            ConstraintType::Equal(left, right) => {
                format!("{} == {}", self.fmt(*left), self.fmt(*right))
            }
            ConstraintType::Subtype { subty, superty } => {
                format!("{} <: {}", self.fmt(*subty), self.fmt(*superty))
            }
            ConstraintType::Join { output, inputs } => {
                format!("join {} {}", self.fmt(*output), self.fmt_many(inputs))
            }
            ConstraintType::Predicate(type_predicate) => format!(
                "Predicate: Cid: {} Args: {}",
                type_predicate.cid.0,
                self.fmt_many(&type_predicate.args)
            ),
            ConstraintType::Arity { expected, found } => format!(
                "len({}) == len({})",
                self.fmt_many(expected),
                self.fmt_many(found)
            ),
            ConstraintType::HasEnoughArgs { expected, found } => format!(
                "len({}) <= len({})",
                self.fmt_many(expected),
                self.fmt_many(found)
            ),
        }
    }

    pub fn get_variance(&self, tid: TypeId, tyvar: TypeVar) -> Option<Variance> {
        match self.types.get(tid.0).expect("TypeIds should be resolvable") {
            Type::Never => None,
            Type::TypeVar(tv) => {
                if tyvar == *tv {
                    Some(Variance::Covariant)
                } else {
                    None
                }
            }
            Type::RigidTypeVar(_) => {
                unreachable!("Rigid Type Vars should not be used before type checking")
            }
            Type::Builtin(_) => None,
            Type::Function(f) => {
                let input_variance = f.input.iter().fold(None, |result, tid| {
                    let variance = self
                        .get_variance(*tid, tyvar)
                        .map(|v| Variance::Contravariant.compose(v));
                    Variance::combine(result, variance)
                });

                let output_variance = f.output.iter().fold(None, |result, tid| {
                    let variance = self
                        .get_variance(*tid, tyvar)
                        .map(|v| Variance::Covariant.compose(v));
                    Variance::combine(result, variance)
                });

                Variance::combine(input_variance, output_variance)
            }
            Type::Pointer(p) => self.get_variance(p.tid, tyvar).map(|v| match p.mutability {
                Mutability::Mutable => Variance::Invariant,
                Mutability::Const => v,
            }),
            Type::Struct(s) => {
                let def = self
                    .structs
                    .get(s.sid.0)
                    .expect("Struct Ids should be resolvable");

                assert!(s.args.len() == def.variances.len());
                s.args
                    .iter()
                    .zip(def.variances.iter())
                    .fold(None, |result, (arg, variance)| {
                        Variance::combine(
                            result,
                            self.get_variance(*arg, tyvar).map(|v| variance.compose(v)),
                        )
                    })
            }
            Type::Enum(e) => {
                let def = self
                    .enums
                    .get(e.eid.0)
                    .expect("EnumIds should be resolvable");

                assert!(e.args.len() == def.variances.len());
                e.args
                    .iter()
                    .zip(def.variances.iter())
                    .fold(None, |result, (arg, variance)| {
                        Variance::combine(
                            result,
                            self.get_variance(*arg, tyvar).map(|v| variance.compose(v)),
                        )
                    })
            }
            Type::Variant(variant_type) => {
                // TODO: We can consider the actual underlying type of the enum variant
                // and check for a variance relation, but that sounds like it sucks at
                // the moment. So I'll come back to this when I need to.
                todo!()
            }
            Type::Type(tid) => self.get_variance(*tid, tyvar).map(|_| Variance::Invariant),
            Type::Tuple(tids) => tids.iter().fold(None, |result, tid| {
                Variance::combine(result, self.get_variance(*tid, tyvar))
            }),
            Type::Projection(_) => todo!(),
        }
    }

    /// Mark a typevar as being rigid.
    ///
    /// A rigid TypeVar can't be unified with anything else. This is used to mark
    /// generic function parameters as being distinct types.
    ///
    /// We want to avoid the situation where
    /// ```haystack
    /// fn swap<A B>(A: a B: b) -> [B A] { b a }
    /// ```
    /// type checks successfully because `A` and `B` can be unified if `A==B`.
    fn solidify_var(&mut self, var: &TypeVar) -> Result<(), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        let rigid_tid = self.intern(Type::RigidTypeVar(*var));
        self.lookup
            .assign_key(*var, Some(rigid_tid))
            .map_err(|_| todo!())
    }

    /// Mark a set of vars as being Rigid
    pub fn solidify_vars<'a, Vars>(&mut self, vars: Vars) -> Result<(), InferenceError<M>>
    where
        Vars: IntoIterator<Item = &'a TypeVar>,
        M: Debug + Clone + PartialEq + Eq,
    {
        vars.into_iter().map(|var| self.solidify_var(var)).collect()
    }

    /// Substitute a TypeVar into Common Form.
    ///
    /// Returns any TypeVars not yet bound through this substitution as well as
    /// the substituted TypeId.
    pub fn substitute_one(&mut self, tid: TypeId) -> (HashSet<TypeVar>, TypeId) {
        match self
            .get(tid)
            .expect("All TypeIds must be present in typestore")
        {
            Type::Builtin(_) | Type::RigidTypeVar(_) | Type::Never => (HashSet::new(), tid),
            Type::Type(tid) => {
                let (unbound, tid) = self.substitute_one(*tid);
                let tid = self.intern(Type::Type(tid));
                (unbound, tid)
            }
            Type::TypeVar(type_var) => {
                // Do union-find substitution to get the next subsitution.
                // The result might require further subsitution.
                match self.lookup.get(*type_var) {
                    Some(tid) => self.substitute_one(*tid),
                    None => (HashSet::from([*type_var]), tid),
                }
            }
            Type::Function(func) => {
                let func = func.clone();
                // Substitute the input and output
                let (unbound_input, input) = self.substitute(&func.input);
                let (unbound_output, output) = self.substitute(&func.output);

                // Combine all unbound variables.
                let unbound = unbound_input.into_iter().chain(unbound_output).collect();

                let new_func = FunctionType { input, output };

                let tid = self.intern(Type::Function(new_func.clone()));
                (unbound, tid)
            }
            Type::Pointer(PointerType { tid, mutability }) => {
                let mutability = *mutability;
                let (unbound, inner_tid) = self.substitute_one(*tid);
                let tid = self.pointer(inner_tid, mutability);
                (unbound, tid)
            }
            Type::Struct(s) => {
                let s = s.clone();
                let (unbound, args) = self.substitute(&s.args);
                let s = StructType { sid: s.sid, args };
                let tid = self.intern(Type::Struct(s));
                (unbound, tid)
            }
            Type::Enum(e) => {
                let e = e.clone();
                let (unbound, args) = self.substitute(&e.args);
                let e = EnumType { eid: e.eid, args };
                let tid = self.intern(Type::Enum(e));
                (unbound, tid)
            }
            Type::Variant(v) => {
                let v = v.clone();
                let (unbound, args) = self.substitute(&v.args);
                let v = VariantType {
                    eid: v.eid,
                    args,
                    variant: v.variant,
                };
                let tid = self.intern(Type::Variant(v));
                (unbound, tid)
            }
            Type::Tuple(tids) => {
                let tids = tids.clone();
                let (unbound, tids) = self.substitute(&tids);
                let tid = self.tuple(tids);
                (unbound, tid)
            }
            Type::Projection(p) => {
                let p = p.clone();
                let (unbound, args) = self.substitute(&p.args);
                let p = Projection {
                    cid: p.cid,
                    args,
                    idx: p.idx,
                };
                let tid = self.intern(Type::Projection(p));
                (unbound, tid)
            }
        }
    }

    /// Substitute all TypeVars into common form
    fn substitute(&mut self, stack: &Stack) -> (HashSet<TypeVar>, Vec<TypeId>) {
        let mut unbound = HashSet::new();
        let mut stk = vec![];

        for tid in stack {
            let (ub, tid) = self.substitute_one(*tid);
            unbound.extend(ub);
            stk.push(tid);
        }

        (unbound, stk)
    }

    pub fn substitute_predicate(
        &mut self,
        predicate: &TypePredicate,
    ) -> (HashSet<TypeVar>, TypePredicate) {
        let (unbound, args) = self.substitute(&predicate.args);
        (
            unbound,
            TypePredicate {
                cid: predicate.cid,
                args,
            },
        )
    }

    /// Substitute all types referenced in an Expression into common form.
    fn substitute_expr<'src>(
        &mut self,
        expr: Expr<'src, (M, TypeId)>,
    ) -> (HashSet<TypeVar>, Expr<'src, (M, TypeId)>)
    where
        M: Debug,
    {
        let (mut unbound, expr_tid) = self.substitute_one(expr.meta.1);

        match expr.expr {
            ExprKind::U8(_)
            | ExprKind::U64(_)
            | ExprKind::Bool(_)
            | ExprKind::String(_)
            | ExprKind::Bind(_)
            | ExprKind::Call
            | ExprKind::UncheckedCast
            | ExprKind::Ident(_)
            | ExprKind::Ref { .. }
            | ExprKind::MemberAccess { .. }
            | ExprKind::NamedMemberAccess { .. }
            | ExprKind::Pack
            | ExprKind::Read
            | ExprKind::Add(_) => (
                unbound,
                Expr {
                    expr: expr.expr,
                    meta: (expr.meta.0, expr_tid),
                },
            ),
            ExprKind::Block(exprs) => {
                let exprs = exprs.into_iter().fold(vec![], |mut exprs, e| {
                    let (ub, e) = self.substitute_expr(e);
                    unbound.extend(ub);
                    exprs.push(e);
                    exprs
                });
                (unbound, Expr::block(exprs, (expr.meta.0, expr_tid)))
            }
            ExprKind::Type(tid) => {
                let (mut ub, innertid) = self.substitute_one(tid);
                unbound.extend(ub);

                (unbound, Expr::typ(innertid, (expr.meta.0, expr_tid)))
            }
            ExprKind::Match(m) => {
                let cases: Vec<_> = m
                    .cases
                    .into_iter()
                    .map(|case| {
                        let (case_unbound, expr) = self.substitute_expr(*case.expr);
                        unbound.extend(case_unbound);
                        MatchCase::new(case.pattern, expr)
                    })
                    .collect();
                let finally = if let Some(expr) = m.finally {
                    let (finally_unbound, expr) = self.substitute_expr(*expr);
                    unbound.extend(finally_unbound);
                    Some(expr)
                } else {
                    None
                };

                (
                    unbound,
                    Expr::match_(MatchExpr::new(cases, finally), (expr.meta.0, expr_tid)),
                )
            }
            ExprKind::If(iff) => {
                let (then_unbound, then) = self.substitute_expr(*iff.then);
                let (otherwise_unbound, otherwise) = self.substitute_expr(*iff.otherwise);

                unbound.extend(then_unbound);
                unbound.extend(otherwise_unbound);

                (unbound, Expr::iff(then, otherwise, (expr.meta.0, expr_tid)))
            }
            ExprKind::Tuple(exprs) => {
                let exprs = exprs.into_iter().fold(vec![], |mut acc, e| {
                    let (ub, e) = self.substitute_expr(e);
                    unbound.extend(ub);
                    acc.push(e);
                    acc
                });

                (unbound, Expr::tuple(exprs, (expr.meta.0, expr_tid)))
            }
        }
    }

    /// Create a new, unused, TypeVar
    pub fn fresh_type_var(&mut self) -> TypeVar {
        self.lookup.new_key()
    }

    /// Create a unique TypeVar to subsitute one that shouldn't be used.
    ///
    /// This is to ensure that generic definitions aren't forced to be unified to a specific type.
    fn freshen_type_var(
        &mut self,
        var: TypeVar,
        fresh: &mut HashMap<TypeVar, TypeId>,
        meta: &M,
    ) -> TypeId {
        if let Some(tid) = fresh.get(&var) {
            *tid
        } else {
            let tyvar = self.fresh_type_var();
            let tid = self.intern(Type::TypeVar(tyvar));
            let origin_tid = self.intern(Type::TypeVar(var));
            assert!(self
                .tyvar_origin
                .insert(tid, (origin_tid, meta.clone()))
                .is_none());
            assert!(fresh.insert(var, tid).is_none());
            tid
        }
    }

    pub fn freshen_impl(&mut self, i: &InterfaceImpl, meta: &M) -> QualifiedInterfaceImpl {
        let mut fresh = HashMap::new();

        i.vars.iter().for_each(|var| {
            self.freshen_type_var(*var, &mut fresh, meta);
        });
        let predicates = i
            .predicates
            .iter()
            .map(|p| self.freshen_predicate(p, &mut fresh, meta))
            .collect();
        let args = self.freshen_many_tids(&i.args, &mut fresh, meta);

        let associated_types = self.freshen_many_tids(&i.associated_types, &mut fresh, meta);

        QualifiedInterfaceImpl {
            cid: i.cid,
            args,
            associated_types,
            predicates,
            coherence: i.coherence,
        }
    }

    fn freshen_variant(
        &mut self,
        variant: &VariantType,
        fresh: &mut HashMap<TypeVar, TypeId>,
        meta: &M,
    ) -> VariantType {
        VariantType {
            eid: variant.eid,
            args: variant
                .args
                .iter()
                .map(|tid| self.freshen_tid(*tid, fresh, meta))
                .collect(),
            variant: variant.variant,
        }
    }

    fn freshen_enum(
        &mut self,
        e: &EnumType,
        fresh: &mut HashMap<TypeVar, TypeId>,
        meta: &M,
    ) -> EnumType {
        EnumType {
            eid: e.eid,
            args: e
                .args
                .iter()
                .map(|tid| self.freshen_tid(*tid, fresh, meta))
                .collect(),
        }
    }

    fn freshen_struct(
        &mut self,
        s: &StructType,
        fresh: &mut HashMap<TypeVar, TypeId>,
        meta: &M,
    ) -> StructType {
        StructType {
            sid: s.sid,
            args: s
                .args
                .iter()
                .map(|tid| self.freshen_tid(*tid, fresh, meta))
                .collect(),
        }
    }

    fn freshen_projection(
        &mut self,
        p: &Projection,
        fresh: &mut HashMap<TypeVar, TypeId>,
        meta: &M,
    ) -> Projection {
        Projection {
            cid: p.cid,
            args: p
                .args
                .iter()
                .map(|tid| self.freshen_tid(*tid, fresh, meta))
                .collect(),
            idx: p.idx,
        }
    }

    fn freshen_tid(
        &mut self,
        tid: TypeId,
        fresh: &mut HashMap<TypeVar, TypeId>,
        meta: &M,
    ) -> TypeId {
        match self.get(tid).expect("All TypeIds should be resolvable") {
            Type::Builtin(_) | Type::RigidTypeVar(_) | Type::Never => tid,
            Type::TypeVar(var) => self.freshen_type_var(*var, fresh, meta),
            Type::Function(func) => {
                let func = func.clone();
                let func = self.freshen_func(&func, fresh, meta);
                self.intern(Type::Function(func))
            }
            Type::Pointer(ptr) => {
                let mutability = ptr.mutability;
                let tid = self.freshen_tid(ptr.tid, fresh, meta);
                self.pointer(tid, mutability)
            }
            Type::Type(tid) => {
                let tid = self.freshen_tid(*tid, fresh, meta);
                self.intern(Type::Type(tid))
            }
            Type::Struct(s) => {
                let s = s.clone();
                let s = StructType {
                    sid: s.sid,
                    args: s
                        .args
                        .into_iter()
                        .map(|tid| self.freshen_tid(tid, fresh, meta))
                        .collect(),
                };
                self.intern(Type::Struct(s))
            }
            Type::Enum(e) => {
                let e = e.clone();
                let e = EnumType {
                    eid: e.eid,
                    args: e
                        .args
                        .into_iter()
                        .map(|tid| self.freshen_tid(tid, fresh, meta))
                        .collect(),
                };
                self.intern(Type::Enum(e))
            }
            Type::Variant(v) => {
                let v = v.clone();
                let v = VariantType {
                    eid: v.eid,
                    args: v
                        .args
                        .into_iter()
                        .map(|tid| self.freshen_tid(tid, fresh, meta))
                        .collect(),
                    variant: v.variant,
                };
                self.intern(Type::Variant(v))
            }
            Type::Tuple(tids) => {
                let tids = tids.clone();
                let tids = self.freshen_many_tids(&tids, fresh, meta);
                self.tuple(tids)
            }
            Type::Projection(p) => {
                let mut p = p.clone();
                p.args = self.freshen_many_tids(&p.args, fresh, meta);
                self.intern(Type::Projection(p))
            }
        }
    }

    /// Given a Scheme, freshen it
    fn freshen(&mut self, scheme: &Scheme, meta: &M) -> QualifiedType {
        let mut fresh = HashMap::new();

        scheme.vars.iter().for_each(|var| {
            self.freshen_type_var(*var, &mut fresh, meta);
        });
        let predicates = scheme
            .predicates
            .iter()
            .map(|p| self.freshen_predicate(p, &mut fresh, meta))
            .collect();
        let tid = self.freshen_tid(scheme.tid, &mut fresh, meta);

        QualifiedType { tid, predicates }
    }

    fn freshen_predicate(
        &mut self,
        predicate: &TypePredicate,
        fresh: &mut HashMap<TypeVar, TypeId>,
        meta: &M,
    ) -> TypePredicate {
        TypePredicate {
            cid: predicate.cid,
            args: predicate
                .args
                .iter()
                .map(|tid| self.freshen_tid(*tid, fresh, meta))
                .collect(),
        }
    }

    /// Given a list of typeids, freshen then all.
    fn freshen_many_tids(
        &mut self,
        tids: &Vec<TypeId>,
        fresh: &mut HashMap<TypeVar, TypeId>,
        meta: &M,
    ) -> Vec<TypeId> {
        tids.into_iter()
            .map(|tid| self.freshen_tid(*tid, fresh, meta))
            .collect()
    }

    /// Freshen a FunctionType
    fn freshen_func(
        &mut self,
        func: &FunctionType,
        fresh: &mut HashMap<TypeVar, TypeId>,
        meta: &M,
    ) -> FunctionType {
        // Create fresh vars for all the types the function is paramaterized over
        let input = self.freshen_many_tids(&func.input, fresh, meta);
        let output = self.freshen_many_tids(&func.output, fresh, meta);

        FunctionType { input, output }
    }

    fn infer_fn_call(
        &mut self,
        func: &FunctionType,
        predicates: Vec<TypePredicate>,
        stack: &mut Stack,
        meta: &M,
    ) -> Vec<Constraint<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        // We're going to defer the check for enough arguments to get as
        // much typing information as possible. So if we don't have enough
        // args, just consume the stack. The error will be reported during
        // constraint checking.
        let argument_constraint = Constraint::new(
            ConstraintType::HasEnoughArgs {
                expected: func.input.clone(),
                found: stack.clone(),
            },
            ConstraintOrigin::FunctionCall {
                input: stack.clone(),
                expected: func.input.clone(),
            },
            meta,
        );
        let split = if stack.len() < func.input.len() {
            stack.split_off(0)
        } else {
            stack.split_off(stack.len() - func.input.len())
        };
        let constraint = ConstraintOrigin::FunctionCall {
            input: split.clone(),
            expected: func.input.clone(),
        };

        let constraints = split
            .into_iter()
            .rev()
            .zip(func.input.iter().rev())
            .map(|(found, expected)| {
                Constraint::new(
                    ConstraintType::Subtype {
                        subty: found,
                        superty: *expected,
                    },
                    constraint.clone(),
                    meta,
                )
            })
            .chain(predicates.into_iter().map(|pred| {
                Constraint::new(ConstraintType::Predicate(pred), constraint.clone(), meta)
            }))
            .chain([argument_constraint]);

        stack.extend(func.output.clone());

        constraints.collect()
    }

    fn infer_ident<'src>(
        &mut self,
        var: Var,
        meta: M,
        stack: &mut Stack,
        env: &mut Environment,
    ) -> Result<(Expr<'src, (M, TypeId)>, Vec<Constraint<M>>), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        let scheme = env.get_var(&var).expect("Vars should be resolvable");
        let inst = self.freshen(scheme, &meta);

        let (tid, constraints) = if let Some(func) = self.get_func(&inst.tid).cloned() {
            (
                inst.tid,
                self.infer_fn_call(&func, inst.predicates, stack, &meta),
            )
        } else {
            // If an identifier _isn't_ a funciton, then add the value to the stack.
            stack.push(inst.tid);
            let tid = inst.tid;
            let constraints = inst
                .predicates
                .into_iter()
                .map(|pred| {
                    Constraint::new(
                        ConstraintType::Predicate(pred),
                        ConstraintOrigin::Identifier(tid),
                        &meta,
                    )
                })
                .collect();
            (tid, constraints)
        };

        let e = Expr::ident(var, (meta, tid));

        Ok((e, constraints))
    }

    fn uncheckec_cast_fn(&mut self) -> FunctionType {
        let from_tyvar = self.fresh_type_var();
        let into_tyvar = self.fresh_type_var();
        let from_tid = self.intern(Type::TypeVar(from_tyvar));
        let into_tid = self.intern(Type::TypeVar(into_tyvar));
        let into_ty_tid = self.intern(Type::Type(into_tid));
        FunctionType {
            input: vec![from_tid, into_ty_tid],
            output: vec![into_tid],
        }
    }

    fn generate_bindings(
        vars: &Vec<TypeVar>,
        args: &Vec<TypeId>,
    ) -> Result<HashMap<TypeVar, TypeId>, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        if vars.len() != args.len() {
            todo!("Error: can't bind everything");
        }

        Ok(HashMap::from_iter(
            vars.iter().zip(args.iter()).map(|(var, tid)| (*var, *tid)),
        ))
    }

    fn instantiate_tid(&mut self, tid: TypeId, bindings: &HashMap<TypeVar, TypeId>) -> TypeId {
        match self.get(tid).expect("Tids should be resolvable").clone() {
            Type::TypeVar(tyvar) => {
                if let Some(tid) = bindings.get(&tyvar) {
                    *tid
                } else {
                    tid
                }
            }
            Type::RigidTypeVar(_) | Type::Builtin(_) | Type::Never => tid,
            Type::Function(function_type) => {
                let input = self.instantiate_tids(&function_type.input, bindings);
                let output = self.instantiate_tids(&function_type.output, bindings);
                self.function(FunctionType { input, output })
            }
            Type::Pointer(p) => {
                let inner_tid = self.instantiate_tid(p.tid, bindings);
                self.pointer(inner_tid, p.mutability)
            }
            Type::Struct(s) => {
                let s = StructType {
                    sid: s.sid,
                    args: self.instantiate_tids(&s.args, bindings),
                };
                self.intern(Type::Struct(s))
            }
            Type::Type(tid) => {
                let tid = self.instantiate_tid(tid, bindings);
                self.intern(Type::Type(tid))
            }
            Type::Enum(_) => todo!(),
            Type::Variant(_) => todo!(),
            Type::Tuple(tids) => {
                let tids = self.instantiate_tids(&tids, bindings);
                self.tuple(tids)
            }
            Type::Projection(_) => todo!(),
        }
    }

    fn instantiate_tids(
        &mut self,
        tids: &Vec<TypeId>,
        bindings: &HashMap<TypeVar, TypeId>,
    ) -> Vec<TypeId> {
        tids.iter()
            .map(|tid| self.instantiate_tid(*tid, bindings))
            .collect()
    }

    fn generate_struct_constructor(
        &mut self,
        tid: TypeId,
    ) -> Result<FunctionType, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        if let Some(Type::Struct(s)) = self.get(tid).cloned() {
            let def = self.get_struct_def(s.sid).clone();
            let bindings = Self::generate_bindings(&def.vars, &s.args)?;
            let members = self.instantiate_tids(&def.members, &bindings);
            // (s.args) -> [s]
            Ok(FunctionType {
                input: members,
                output: vec![tid],
            })
        } else {
            todo!("Expected a struct, found {}", self.fmt(tid));
        }
    }

    fn generate_variant_constructor(
        &mut self,
        tid: TypeId,
    ) -> Result<FunctionType, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        if let Some(Type::Variant(v)) = self.get(tid).cloned() {
            let def = self.get_enum_def(v.eid).clone();
            assert!(v.variant < def.variants.len());
            let bindings = Self::generate_bindings(&def.vars, &v.args)?;

            let mut input = vec![];
            if let Some(tid) = def.variants[v.variant] {
                input.push(self.instantiate_tid(tid, &bindings));
            };

            // (s.args) -> [s]
            Ok(FunctionType {
                input,
                output: vec![tid],
            })
        } else {
            todo!("Expected a struct, found {}", self.fmt(tid));
        }
    }

    fn generate_constructor(&mut self, tid: TypeId) -> Result<FunctionType, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        match self.get(tid).expect("TypeIds should be resolvable") {
            Type::Struct(_) => self.generate_struct_constructor(tid),
            Type::Variant(_) => self.generate_variant_constructor(tid),
            Type::Tuple(tids) => Ok(FunctionType {
                input: tids.clone(),
                output: vec![tid],
            }),
            Type::Enum(_)
            | Type::TypeVar(_)
            | Type::RigidTypeVar(_)
            | Type::Builtin(_)
            | Type::Function(_)
            | Type::Pointer(_)
            | Type::Type(_)
            | Type::Never
            | Type::Projection(_) => todo!("Can't pack into a {}", self.fmt(tid)),
        }
    }

    /// Infer what type constraints must be placed on an Expression.
    ///
    /// Expressions may push/pop to/from the stack
    /// Expressions can introduce new variables to the Environment
    ///
    /// Expressions are also marked with all their constraints in addition to
    /// whatever metadata they had beforehand.
    fn infer<'src, Lookup>(
        &mut self,
        expr: Expr<'src, M>,
        stack: &mut Stack,
        env: &mut Environment,
        lookup: &Lookup,
    ) -> Result<(Expr<'src, (M, TypeId)>, Vec<Constraint<M>>), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
        Lookup: MemberLookup,
    {
        let e = match expr.expr {
            ExprKind::U8(val) => {
                let u8_tid = self.intern(Type::u8());
                let func = FunctionType {
                    input: vec![],
                    output: vec![u8_tid],
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::u8(val, (expr.meta.clone(), self.function(func)));
                Ok((e, constraints))
            }
            ExprKind::U64(val) => {
                let u8_tid = self.intern(Type::u64());
                let func = FunctionType {
                    input: vec![],
                    output: vec![u8_tid],
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::u64(val, (expr.meta.clone(), self.function(func)));
                Ok((e, constraints))
            }
            ExprKind::Bool(val) => {
                let bool_tid = self.intern(Type::bool());
                let func = FunctionType {
                    input: vec![],
                    output: vec![bool_tid],
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::bool(val, (expr.meta, self.function(func)));
                Ok((e, constraints))
            }
            ExprKind::String(s) => {
                let str_tid = self.str_tid.expect("Str tid must be provided");
                let func = FunctionType {
                    input: vec![],
                    output: vec![str_tid],
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::string(s, (expr.meta, self.function(func)));
                Ok((e, constraints))
            }
            ExprKind::Bind(vars) => {
                if stack.len() < vars.len() {
                    todo!("Too few elements to bind")
                }

                let popped = stack.split_off(stack.len() - vars.len());
                popped
                    .iter()
                    .zip(vars.iter())
                    .for_each(|(tid, var)| assert_eq!(env.bind(*var, *tid), None));

                let tid = self.function(FunctionType {
                    input: popped,
                    output: vec![],
                });
                let e = Expr::bind(vars, (expr.meta, tid));
                Ok((e, vec![]))
            }
            ExprKind::Ident(var) => self.infer_ident(var, expr.meta, stack, env),
            ExprKind::Ref { var, mutability } => {
                let scheme = env.get_var(&var).expect("Vars should be resolvable");
                let inst = self.freshen(scheme, &expr.meta);

                let tid = if let Some(func) = self.get_func(&inst.tid).cloned() {
                    if mutability != Mutability::Const {
                        return Err(InferenceError::mutable_ref_to_fn(
                            var,
                            &ConstraintOrigin::TakeRef { var, mutability },
                            &expr.meta,
                        ));
                    }
                    self.function(func)
                } else {
                    self.pointer(inst.tid, mutability)
                };

                stack.push(tid);
                let e_tid = self.function(FunctionType {
                    input: vec![],
                    output: vec![tid],
                });
                let e = Expr::take_ref(var, mutability, (expr.meta, e_tid));

                Ok((e, vec![]))
            }
            ExprKind::Block(exprs) => {
                let input = stack.clone();

                let mut local_env = env.clone();
                let (exprs, constraints) = exprs
                    .into_iter()
                    .map(|e| self.infer(e, stack, &mut local_env, lookup))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .fold(
                        (vec![], vec![]),
                        |(mut exprs, mut constraints), (e, cons)| {
                            exprs.push(e);
                            constraints.extend(cons);
                            (exprs, constraints)
                        },
                    );

                let output = stack.clone();
                let tid = self.function(FunctionType { input, output });
                let e = Expr::block(exprs, (expr.meta, tid));
                Ok((e, constraints))
            }
            ExprKind::Call => {
                if stack.is_empty() {
                    todo!("Can't call when there's nothing on the stack!");
                }

                let tid = stack.pop().unwrap();
                if let Some(Type::Function(mut func)) = self.get(tid).cloned() {
                    let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                    func.input.push(tid);
                    let expr_tid = self.function(func);
                    let e = Expr::call((expr.meta, expr_tid));
                    Ok((e, constraints))
                } else {
                    todo!("Error can't call a non-function: {}", self.fmt(tid))
                }
            }
            ExprKind::Type(tid) => {
                assert!(matches!(self.types[tid.0], Type::Type(_)));
                stack.push(tid);

                let expr_tid = self.function(FunctionType {
                    input: vec![],
                    output: vec![tid],
                });
                let e = Expr::typ(tid, (expr.meta, expr_tid));
                Ok((e, vec![]))
            }
            ExprKind::UncheckedCast => {
                let func = self.uncheckec_cast_fn();
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let expr_tid = self.function(func);
                let e = Expr::unchecked_cast((expr.meta, expr_tid));
                Ok((e, constraints))
            }
            ExprKind::Pack => {
                if stack.is_empty() {
                    todo!("Expected the type to pack to on top of stack");
                }

                let ty_tid = stack.pop().unwrap();
                if let Some(Type::Type(tid)) = self.get(ty_tid).cloned() {
                    let mut constructor = self.generate_constructor(tid)?;
                    let constraints = self.infer_fn_call(&constructor, vec![], stack, &expr.meta);

                    constructor.input.push(ty_tid);
                    let expr_tid = self.function(constructor);
                    let e = Expr::pack((expr.meta, expr_tid));
                    Ok((e, constraints))
                } else {
                    todo!(
                        "Expected a type on the top of the stack. found: {}",
                        self.fmt(ty_tid)
                    )
                }
            }
            ExprKind::NamedMemberAccess { var, member } => {
                let scheme = env.get_var(&var).expect("Vars should be resolvable");
                let inst = self.freshen(scheme, &expr.meta);

                let Some(Type::Struct(s)) = self.get(inst.tid) else {
                    todo!(
                        "{:?}: Error can't get member for non-struct: {}",
                        expr.meta,
                        self.fmt(inst.tid)
                    )
                };

                let Some(member_idx) = lookup.get_member_idx(s.sid, member) else {
                    todo!("Error unknown member")
                };

                let expr = Expr::member_access(var, member_idx, expr.meta);
                self.infer(expr, stack, env, lookup)
            }
            ExprKind::MemberAccess { var, member } => {
                let scheme = env.get_var(&var).expect("Vars should be resolvable");
                let inst = self.freshen(scheme, &expr.meta);

                let constraints = inst
                    .predicates
                    .into_iter()
                    .map(|pred| {
                        Constraint::new(
                            ConstraintType::Predicate(pred),
                            ConstraintOrigin::MemberAccess { var, member },
                            &expr.meta,
                        )
                    })
                    .collect();

                if let Some(Type::Struct(s)) = self.get(inst.tid) {
                    let def = self.get_struct_def(s.sid).clone();
                    let bindings = Self::generate_bindings(&def.vars, &s.args)?;
                    let members = self.instantiate_tids(&def.members, &bindings);
                    let tid = members[member];
                    stack.push(tid);

                    let expr_tid = self.function(FunctionType {
                        input: vec![],
                        output: vec![tid],
                    });
                    let e = Expr::member_access(var, member, (expr.meta, expr_tid));

                    Ok((e, constraints))
                } else {
                    todo!("Error: Can't access member of non-struct type")
                }
            }
            ExprKind::Match(m) => {
                let input = stack.clone();

                // pop an enum off of the stack.
                if stack.is_empty() {
                    todo!("Error: Expected an enum")
                }

                let tid = stack.pop().unwrap();
                let enum_ty = if let Some(Type::Enum(e)) = self.get(tid).cloned() {
                    e
                } else {
                    todo!("Error: {} is not an enum", self.fmt(tid));
                };

                let enum_def = self.get_enum_def(enum_ty.eid).clone();
                let bindings = Self::generate_bindings(&enum_def.vars, &enum_ty.args)?;

                // Make sure that all cases operate on the same enum type
                let mut variants_covered = vec![false; enum_def.variants.len()];
                let mut inner_tids = vec![None; m.cases.len()];
                if !m
                    .cases
                    .iter()
                    .enumerate()
                    .all(|(i, case)| match case.pattern {
                        MatchPattern::Variant(type_id) => {
                            let variant = if let Some(Type::Variant(v)) = self.get(type_id) {
                                assert!(v.variant < variants_covered.len());
                                if v.eid != enum_ty.eid {
                                    return false;
                                }
                                v.variant
                            } else {
                                return false;
                            };

                            if variants_covered[variant] {
                                // TODO: maybe handle this as a separate error?
                                return false;
                            }

                            variants_covered[variant] = true;
                            let tid = enum_def.variants[variant];
                            let tid = tid.map(|tid| self.instantiate_tid(tid, &bindings));
                            inner_tids[i] = tid;

                            true
                        }
                    })
                {
                    todo!("Case doesn't match enum type")
                }

                if m.finally.is_none() && !variants_covered.iter().all(|b| *b) {
                    todo!("Missing case: {variants_covered:?}")
                }

                let mut stacks = vec![];
                let mut cases = vec![];
                let mut constraints = vec![];
                for (i, case) in m.cases.into_iter().enumerate() {
                    let mut local_env = env.clone();
                    let mut local_stack = stack.clone();

                    if let Some(case_tid) = inner_tids[i] {
                        local_stack.push(case_tid);
                    }

                    let (expr, constrs) =
                        self.infer(*case.expr, &mut local_stack, &mut local_env, lookup)?;
                    constraints.extend(constrs);
                    stacks.push(local_stack);
                    cases.push(MatchCase::new(case.pattern, expr));
                }

                let finally = if let Some(expr) = m.finally {
                    let mut local_env = env.clone();
                    let mut local_stack = stack.clone();
                    let (expr, constrs) =
                        self.infer(*expr, &mut local_stack, &mut local_env, lookup)?;
                    constraints.extend(constrs);
                    stacks.push(local_stack);
                    Some(expr)
                } else {
                    None
                };

                if stacks.is_empty() {
                    todo!("Error: Empty match")
                }

                let len = stacks[0].len();
                if !stacks.iter().all(|stack| stack.len() == len) {
                    stacks
                        .iter()
                        .for_each(|stack| eprintln!("{}", self.fmt_many(stack)));

                    todo!("Error: Stack shapes differ")
                }

                let fresh_stack: Vec<_> = (0..len)
                    .map(|_| {
                        let tyvar = self.fresh_type_var();
                        self.intern(Type::TypeVar(tyvar))
                    })
                    .collect();

                for (i, tid) in fresh_stack.iter().enumerate() {
                    let inputs = stacks.iter().map(|stack| stack[i]).collect();
                    constraints.push(Constraint::new(
                        ConstraintType::Join {
                            output: *tid,
                            inputs,
                        },
                        ConstraintOrigin::Branching {
                            fresh_stack: fresh_stack.clone(),
                            stacks: stacks.clone(),
                        },
                        &expr.meta,
                    ));
                }

                let expr_tid = self.function(FunctionType {
                    input,
                    output: fresh_stack.clone(),
                });
                let e = Expr::match_(MatchExpr::new(cases, finally), (expr.meta, expr_tid));

                *stack = fresh_stack;

                Ok((e, constraints))
            }
            ExprKind::If(iff) => {
                let input = stack.clone();

                // pop an enum off of the stack.
                if stack.is_empty() {
                    todo!("Error: Expected an enum")
                }

                let tid = stack.pop().unwrap();
                let mut constraints = vec![Constraint::new(
                    ConstraintType::Equal(tid, self.intern(Type::bool())),
                    ConstraintOrigin::IfCondition,
                    &expr.meta,
                )];

                let mut then_stack = stack.clone();
                let mut then_env = env.clone();
                let (then, constrs) =
                    self.infer(*iff.then, &mut then_stack, &mut then_env, lookup)?;
                constraints.extend(constrs);

                let mut otherwise_stack = stack.clone();
                let mut otherwise_env = env.clone();
                let (otherwise, constrs) = self.infer(
                    *iff.otherwise,
                    &mut otherwise_stack,
                    &mut otherwise_env,
                    lookup,
                )?;
                constraints.extend(constrs);

                if then_stack.len() != otherwise_stack.len() {
                    todo!("Error: Stack shapes differ")
                }

                let fresh_stack: Vec<_> = (0..then_stack.len())
                    .map(|_| {
                        let tyvar = self.fresh_type_var();
                        self.intern(Type::TypeVar(tyvar))
                    })
                    .collect();

                for (i, tid) in fresh_stack.iter().enumerate() {
                    let inputs = [&then_stack, &otherwise_stack]
                        .iter()
                        .map(|stack| stack[i])
                        .collect();
                    constraints.push(Constraint::new(
                        ConstraintType::Join {
                            output: *tid,
                            inputs,
                        },
                        ConstraintOrigin::Branching {
                            fresh_stack: fresh_stack.clone(),
                            stacks: vec![then_stack.clone(), otherwise_stack.clone()],
                        },
                        &expr.meta,
                    ));
                }

                let expr_tid = self.function(FunctionType {
                    input,
                    output: fresh_stack.clone(),
                });
                let e = Expr::iff(then, otherwise, (expr.meta, expr_tid));

                *stack = fresh_stack;

                Ok((e, constraints))
            }
            ExprKind::Tuple(exprs) => {
                let mut inner_stack = vec![];

                let mut local_env = env.clone();
                let (exprs, constraints) = exprs
                    .into_iter()
                    .map(|e| self.infer(e, &mut inner_stack, &mut local_env, lookup))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_iter()
                    .fold(
                        (vec![], vec![]),
                        |(mut exprs, mut constraints), (e, cons)| {
                            exprs.push(e);
                            constraints.extend(cons);
                            (exprs, constraints)
                        },
                    );

                let tuple_tid = self.tuple(inner_stack);

                stack.push(tuple_tid);

                let tid = self.function(FunctionType {
                    input: vec![],
                    output: vec![tuple_tid],
                });
                let e = Expr::tuple(exprs, (expr.meta, tid));
                Ok((e, constraints))
            }
            ExprKind::Read => todo!(),
            ExprKind::Add(tid) => {
                let func = FunctionType {
                    input: vec![tid, tid],
                    output: vec![tid],
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::add(tid, (expr.meta.clone(), self.function(func)));
                Ok((e, constraints))
            }
        };

        e
    }

    pub fn normalize_ty(
        &mut self,
        env: &Environment,
        tid: TypeId,
        meta: &M,
    ) -> Result<(Type, TypeId), InferenceErrorKind> {
        match self.get(tid).expect("Type Ids should be resolvable") {
            Type::Never => Ok((Type::Never, tid)),
            Type::Builtin(b) => Ok((Type::Builtin(*b), tid)),
            Type::TypeVar(var) => match self.lookup.get(*var).clone() {
                Some(tid) => self.normalize_ty(env, tid, meta),
                None => Ok((Type::TypeVar(*var), tid)),
            },
            Type::RigidTypeVar(var) => Ok((Type::RigidTypeVar(*var), tid)),
            Type::Pointer(ptr) => {
                let ptr = ptr.clone();
                let (_, inner_tid) = self.normalize_ty(env, ptr.tid, meta)?;
                let ty = Type::Pointer(PointerType {
                    tid: inner_tid,
                    mutability: ptr.mutability,
                });

                Ok((ty.clone(), self.intern(ty)))
            }
            Type::Function(func) => {
                let func = func.clone();
                let input = func
                    .input
                    .iter()
                    .map(|tid| self.normalize_ty(env, *tid, meta).map(|(_, tid)| tid))
                    .collect::<Result<_, _>>()?;
                let output = func
                    .output
                    .iter()
                    .map(|tid| self.normalize_ty(env, *tid, meta).map(|(_, tid)| tid))
                    .collect::<Result<_, _>>()?;

                let ty = Type::Function(FunctionType { input, output });
                Ok((ty.clone(), self.intern(ty)))
            }
            Type::Type(inner_tid) => {
                let (_, inner_tid) = self.normalize_ty(env, *inner_tid, meta)?;
                let ty = Type::Type(inner_tid);
                let tid = self.intern(ty);
                Ok((Type::Type(inner_tid), tid))
            }
            Type::Struct(s) => {
                let s = s.clone();
                let s = StructType {
                    sid: s.sid,
                    args: s
                        .args
                        .iter()
                        .map(|tid| self.normalize_ty(env, *tid, meta).map(|(_, tid)| tid))
                        .collect::<Result<_, _>>()?,
                };

                let ty = Type::Struct(s);
                let tid = self.intern(ty.clone());

                Ok((ty, tid))
            }
            Type::Enum(e) => {
                let e = e.clone();
                let e = EnumType {
                    eid: e.eid,
                    args: e
                        .args
                        .iter()
                        .map(|tid| self.normalize_ty(env, *tid, meta).map(|(_, tid)| tid))
                        .collect::<Result<_, _>>()?,
                };
                let ty = Type::Enum(e);
                let tid = self.intern(ty.clone());
                Ok((ty, tid))
            }
            Type::Variant(v) => {
                let v = v.clone();
                let v = VariantType {
                    eid: v.eid,
                    args: v
                        .args
                        .iter()
                        .map(|tid| self.normalize_ty(env, *tid, meta).map(|(_, tid)| tid))
                        .collect::<Result<_, _>>()?,
                    variant: v.variant,
                };

                let ty = Type::Variant(v);
                let tid = self.intern(ty.clone());

                Ok((ty, tid))
            }
            Type::Tuple(tids) => {
                let tids = tids.clone();
                let tids = tids
                    .iter()
                    .map(|tid| self.normalize_ty(env, *tid, meta).map(|(_, tid)| tid))
                    .collect::<Result<_, _>>()?;
                let ty = Type::Tuple(tids);
                let tid = self.intern(ty.clone());
                Ok((ty, tid))
            }
            Type::Projection(p) => {
                let p = p.clone();
                if let Some(QualifiedInterfaceImpl {
                    associated_types, ..
                }) = self.get_matching_impl(env, p.cid, &p.args, meta)?
                {
                    assert!(
                        p.idx < associated_types.len(),
                        "{} should be less than {}",
                        p.idx,
                        associated_types.len()
                    );
                    self.normalize_ty(env, associated_types[p.idx], meta)
                } else {
                    let args = p
                        .args
                        .iter()
                        .map(|tid| self.normalize_ty(env, *tid, meta).map(|(_, tid)| tid))
                        .collect::<Result<_, _>>()?;
                    let ty = Type::Projection(Projection {
                        cid: p.cid,
                        args,
                        idx: p.idx,
                    });
                    let tid = self.intern(ty.clone());
                    Ok((ty, tid))
                }
            }
        }
    }

    fn unify_typevar(&mut self, var: TypeVar, tid: TypeId) -> Result<(), InferenceErrorKind>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        self.lookup.assign_key(var, Some(tid)).map_err(|_| todo!())
    }

    fn type_is_c_vararg_compatible(&mut self, env: &Environment, tid: TypeId, meta: &M) -> bool {
        let Ok((ty, _)) = self.normalize_ty(env, tid, meta) else {
            return false;
        };
        match ty {
            Type::Tuple(tids) => tids.into_iter().all(|tid| {
                let Ok((ty, _)) = self.normalize_ty(env, tid, meta) else {
                    return false;
                };
                ty.is_c_element()
            }),
            ty => ty.is_c_element(),
        }
    }

    pub fn unify_ty_ty(
        &mut self,
        env: &Environment,
        unnorm_left: TypeId,
        unnorm_right: TypeId,
        meta: &M,
    ) -> Result<(), InferenceErrorKind>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        let (left, left_tid) = self.normalize_ty(env, unnorm_left, meta)?;
        let (right, right_tid) = self.normalize_ty(env, unnorm_right, meta)?;

        match (left, right) {
            (a, b) if a == b => Ok(()),
            (Type::Builtin(left), Type::Builtin(right)) if left == right => Ok(()),
            (Type::Builtin(BuiltinType::CVarArgs), _)
                if self.type_is_c_vararg_compatible(env, right_tid, meta) =>
            {
                Ok(())
            }
            (_, Type::Builtin(BuiltinType::CVarArgs)) => {
                unreachable!("Users shouldn't be able to construct `...`")
            }
            (Type::Builtin(_), Type::Builtin(_)) => Err(InferenceErrorKind::TypesIncompatible {
                expected: left_tid,
                found: right_tid,
            }),
            (Type::RigidTypeVar(left), Type::RigidTypeVar(right)) if left == right => Ok(()),
            (Type::TypeVar(left), Type::TypeVar(right)) => self
                .lookup
                .unify_keys(left, right)
                .map_err(|UnifiactionError(_, _)| todo!()),
            (Type::TypeVar(var), _) => self.unify_typevar(var, right_tid),
            (_, Type::TypeVar(var)) => self.unify_typevar(var, left_tid),
            (Type::Type(left), Type::Type(right)) => self.unify_ty_ty(env, left, right, meta),
            (Type::Type(_), _) | (_, Type::Type(_)) => Err(InferenceErrorKind::TypesIncompatible {
                expected: left_tid,
                found: right_tid,
            }),
            (Type::RigidTypeVar(var), _) => Err(InferenceErrorKind::TypeParameterConstrained {
                var,
                tid: right_tid,
            }),
            (_, Type::RigidTypeVar(var)) => {
                Err(InferenceErrorKind::TypeParameterConstrained { var, tid: left_tid })
            }
            (
                Type::Pointer(PointerType {
                    mutability: mut_left,
                    ..
                }),
                Type::Pointer(PointerType {
                    mutability: mut_right,
                    ..
                }),
            ) if mut_left != mut_right => Err(InferenceErrorKind::TypesIncompatible {
                expected: left_tid,
                found: right_tid,
            }),
            (
                Type::Pointer(PointerType {
                    mutability: mut_left,
                    tid: left_inner_tid,
                }),
                Type::Pointer(PointerType {
                    mutability: mut_right,
                    tid: right_inner_tid,
                }),
            ) if mut_left == mut_right => {
                self.unify_ty_ty(env, left_inner_tid, right_inner_tid, meta)
            }
            (Type::Pointer(_), _) | (_, Type::Pointer(_)) => {
                Err(InferenceErrorKind::TypesIncompatible {
                    expected: left_tid,
                    found: right_tid,
                })
            }
            (Type::Variant(left), Type::Variant(right)) => {
                if left.eid != right.eid {
                    todo!("Type Conflict. Different enum types")
                }

                if left.variant != right.variant {
                    return Err(InferenceErrorKind::TypesIncompatible {
                        expected: left_tid,
                        found: right_tid,
                    });
                }

                assert!(left.args.len() == right.args.len());
                left.args
                    .iter()
                    .zip(right.args.iter())
                    .map(|(left, right)| self.unify_ty_ty(env, *left, *right, meta))
                    .collect::<Result<(), InferenceErrorKind>>()?;

                Ok(())
            }
            (Type::Enum(_), Type::Variant(_)) | (Type::Variant(_), Type::Enum(_)) => {
                Err(InferenceErrorKind::TypesIncompatible {
                    expected: left_tid,
                    found: right_tid,
                })
            }
            (Type::Enum(left), Type::Enum(right)) => {
                if left.eid != right.eid {
                    return Err(InferenceErrorKind::TypesIncompatible {
                        expected: left_tid,
                        found: right_tid,
                    });
                }

                assert!(left.args.len() == right.args.len());
                for (left, right) in left.args.into_iter().zip(right.args.into_iter()) {
                    self.unify_ty_ty(env, left, right, meta)?;
                }

                Ok(())
            }
            (Type::Enum(_), _) | (_, Type::Enum(_)) => Err(InferenceErrorKind::TypesIncompatible {
                expected: left_tid,
                found: right_tid,
            }),
            (Type::Variant(_), _) | (_, Type::Variant(_)) => {
                Err(InferenceErrorKind::TypesIncompatible {
                    expected: left_tid,
                    found: right_tid,
                })
            }
            (Type::Struct(left), Type::Struct(right)) => {
                if left.sid != right.sid {
                    return Err(InferenceErrorKind::TypesIncompatible {
                        expected: left_tid,
                        found: right_tid,
                    });
                }

                assert!(left.args.len() == right.args.len());
                for (left, right) in left.args.into_iter().zip(right.args.into_iter()) {
                    self.unify_ty_ty(env, left, right, meta)?;
                }

                Ok(())
            }
            (Type::Struct(_), _) | (_, Type::Struct(_)) => {
                Err(InferenceErrorKind::TypesIncompatible {
                    expected: left_tid,
                    found: right_tid,
                })
            }
            (Type::Tuple(left), Type::Tuple(right)) => {
                if left.len() != right.len() {
                    return Err(InferenceErrorKind::TypesIncompatible {
                        expected: left_tid,
                        found: right_tid,
                    });
                }

                for (left, right) in left.into_iter().zip(right.into_iter()) {
                    self.unify_ty_ty(env, left, right, meta)?;
                }

                Ok(())
            }
            (Type::Tuple(_), _) | (_, Type::Tuple(_)) => {
                Err(InferenceErrorKind::TypesIncompatible {
                    expected: left_tid,
                    found: right_tid,
                })
            }
            (Type::Function(left), Type::Function(right)) => {
                if (left.input.len() != right.input.len())
                    || (left.output.len() != right.output.len())
                {
                    return Err(InferenceErrorKind::TypesIncompatible {
                        expected: left_tid,
                        found: right_tid,
                    });
                }

                for (left, right) in left.input.into_iter().zip(right.input.into_iter()) {
                    self.unify_ty_ty(env, left, right, meta)?;
                }

                for (left, right) in left.output.into_iter().zip(right.output.into_iter()) {
                    self.unify_ty_ty(env, left, right, meta)?;
                }

                Ok(())
            }
            (_, _) => todo!("Unify {}, {}", self.fmt(left_tid), self.fmt(right_tid)),
        }
    }

    fn check_subtype_args(
        &mut self,
        env: &Environment,
        variances: &[Variance],
        sub_args: &[TypeId],
        super_args: &[TypeId],
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Result<(), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        assert!(variances.len() == sub_args.len());
        assert!(variances.len() == super_args.len());

        for ((variance, sub_tid), super_tid) in variances.iter().zip(sub_args).zip(super_args) {
            let tid = match variance {
                Variance::Covariant => {
                    self.check_subtype(env, *sub_tid, *super_tid, origin, meta)?
                }
                Variance::Invariant => {
                    self.unify_ty_ty(env, *super_tid, *sub_tid, meta)
                        .map_err(|kind| InferenceError {
                            kind,
                            origin: origin.clone(),
                            meta: meta.clone(),
                        })?
                }
                Variance::Contravariant => self
                    .check_subtype(env, *super_tid, *sub_tid, origin, meta)
                    .map_err(|e| match e.kind {
                        InferenceErrorKind::TypesIncompatible { expected, found } => {
                            InferenceError::types_incompatible(found, expected, origin, &e.meta)
                        }
                        _ => e,
                    })?,
            };
        }

        Ok(())
    }

    pub fn check_subtype(
        &mut self,
        env: &Environment,
        subty: TypeId,
        superty: TypeId,
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Result<(), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        let (subty, subty_tid) =
            self.normalize_ty(env, subty, meta)
                .map_err(|kind| InferenceError {
                    kind,
                    origin: origin.clone(),
                    meta: meta.clone(),
                })?;
        let (superty, superty_tid) =
            self.normalize_ty(env, superty, meta)
                .map_err(|kind| InferenceError {
                    kind,
                    origin: origin.clone(),
                    meta: meta.clone(),
                })?;

        match (subty, superty) {
            (Type::Never, _) => Ok(()),
            (Type::Pointer(subty_inner), Type::Pointer(superty_inner)) => {
                match (subty_inner.mutability, superty_inner.mutability) {
                    (Mutability::Mutable, Mutability::Mutable) => {
                        // &mut T <: &mut T
                        // iff T == T
                        self.unify_ty_ty(env, superty_tid, subty_tid, meta)
                            .map_err(|kind| InferenceError {
                                kind,
                                origin: origin.clone(),
                                meta: meta.clone(),
                            })
                    }
                    (Mutability::Mutable, Mutability::Const)
                    | (Mutability::Const, Mutability::Const) => {
                        // A <: B
                        // ---------
                        // &mut A <: &B
                        // &A <: &B
                        self.check_subtype(env, subty_inner.tid, superty_inner.tid, origin, meta)
                    }
                    (Mutability::Const, Mutability::Mutable) => Err(
                        InferenceError::types_incompatible(superty_tid, subty_tid, origin, meta),
                    ),
                }
            }
            (Type::Variant(v), Type::Enum(e)) if v.eid == e.eid => {
                assert!(v.args.len() == e.args.len());
                let variances = self.get_enum_def(e.eid).variances.clone();
                self.check_subtype_args(env, &variances, &v.args, &e.args, origin, meta)
            }
            (Type::Enum(sub_e), Type::Enum(super_e)) if sub_e.eid == super_e.eid => {
                let variances = self.get_enum_def(sub_e.eid).variances.clone();
                self.check_subtype_args(env, &variances, &sub_e.args, &super_e.args, origin, meta)
            }
            (Type::Variant(sub_v), Type::Variant(super_v))
                if sub_v.eid == super_v.eid && sub_v.variant == super_v.variant =>
            {
                let variances = self.get_enum_def(sub_v.eid).variances.clone();
                self.check_subtype_args(env, &variances, &sub_v.args, &super_v.args, origin, meta)
            }
            (Type::Struct(sub_s), Type::Struct(super_s)) if sub_s.sid == super_s.sid => {
                let variances = self.get_struct_def(sub_s.sid).variances.clone();
                self.check_subtype_args(env, &variances, &sub_s.args, &super_s.args, origin, meta)
            }
            (Type::Tuple(sub), Type::Tuple(sup)) if sub.len() == sup.len() => {
                for (sub, sup) in sub.into_iter().zip(sup.into_iter()) {
                    self.check_subtype(env, sub, sup, origin, meta)?;
                }

                Ok(())
            }
            (Type::Function(sub_f), Type::Function(super_f)) => {
                if sub_f.input.len() != super_f.input.len()
                    || sub_f.output.len() != super_f.output.len()
                {
                    todo!("Incompatible arity");
                }
                for (subty, superty) in sub_f.input.iter().zip(super_f.input.iter()) {
                    self.check_subtype(env, *superty, *subty, origin, meta)?;
                }
                for (subty, superty) in sub_f.output.iter().zip(super_f.output.iter()) {
                    self.check_subtype(env, *subty, *superty, origin, meta)?;
                }
                Ok(())
            }
            (_, _) => self
                .unify_ty_ty(env, superty_tid, subty_tid, meta)
                .map_err(|kind| InferenceError {
                    kind,
                    origin: origin.clone(),
                    meta: meta.clone(),
                }),
        }
    }

    fn get_matching_impl<'env>(
        &mut self,
        env: &'env Environment,
        cid: TypeClassId,
        args: &Vec<TypeId>,
        meta: &M,
    ) -> Result<Option<QualifiedInterfaceImpl>, InferenceErrorKind> {
        let (unbound, args) = self.substitute(&args);
        if !unbound.is_empty() {
            return Ok(None);
        }

        match env.get_impls(&cid) {
            Some(impls) => {
                let mut matches: Vec<_> = impls
                    .iter()
                    .filter_map(|i| {
                        let i = self.freshen_impl(i, meta);
                        assert!(i.args.len() == args.len());

                        let types_match =
                            i.args.iter().zip(args.iter()).all(|(expected, found)| {
                                self.unify_ty_ty(env, *expected, *found, meta).is_ok()
                            });

                        if !types_match {
                            return None;
                        }

                        if i.predicates.iter().all(|p| {
                            if let Ok(Some(_)) = self.get_matching_impl(env, p.cid, &p.args, meta) {
                                true
                            } else {
                                false
                            }
                        }) {
                            Some(i)
                        } else {
                            None
                        }
                    })
                    .collect();

                match matches.len() {
                    0 => Ok(None),
                    1 => Ok(Some(matches.pop().unwrap())),
                    _ => {
                        if matches
                            .iter()
                            .all(|i| i.coherence == Coherence::Overlapping)
                        {
                            Ok(Some(matches.pop().unwrap()))
                        } else {
                            todo!("Multiple matching implementations with unique coherence");
                        }
                    }
                }
            }
            None => Ok(None),
        }
    }

    fn check_predicate(
        &mut self,
        env: &Environment,
        predicate: &TypePredicate,
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Result<(), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        match self
            .get_matching_impl(env, predicate.cid, &predicate.args, meta)
            .map_err(|kind| InferenceError {
                kind,
                origin: origin.clone(),
                meta: meta.clone(),
            })? {
            Some(_) => Ok(()),
            None => {
                let (_, args) = self.substitute(&predicate.args);
                Err(InferenceError::predicate_not_met(
                    predicate.cid,
                    args,
                    origin,
                    meta,
                ))
            }
        }
    }

    fn meet_args(
        &mut self,
        env: &Environment,
        variances: &[Variance],
        left: &[TypeId],
        right: &[TypeId],
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Result<Vec<TypeId>, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        assert!(variances.len() == left.len());
        assert!(variances.len() == right.len());

        let mut args = vec![];
        for ((variance, left), right) in variances.iter().zip(left).zip(right) {
            let tid = match variance {
                Variance::Covariant => self.meet_ty_ty(env, *left, *right, origin, meta)?,
                Variance::Invariant => {
                    self.unify_ty_ty(env, *left, *right, meta)
                        .map_err(|kind| InferenceError {
                            kind,
                            origin: origin.clone(),
                            meta: meta.clone(),
                        })?;
                    self.normalize_ty(env, *left, meta)
                        .map_err(|kind| InferenceError {
                            kind,
                            origin: origin.clone(),
                            meta: meta.clone(),
                        })?
                        .1
                }
                Variance::Contravariant => self.join_ty_ty(env, *left, *right, origin, meta)?,
            };

            args.push(tid)
        }
        Ok(args)
    }

    fn meet_ty_ty(
        &mut self,
        env: &Environment,
        left: TypeId,
        right: TypeId,
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Result<TypeId, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        let (left, left_tid) =
            self.normalize_ty(env, left, meta)
                .map_err(|kind| InferenceError {
                    kind,
                    origin: origin.clone(),
                    meta: meta.clone(),
                })?;
        let (right, right_tid) =
            self.normalize_ty(env, right, meta)
                .map_err(|kind| InferenceError {
                    kind,
                    origin: origin.clone(),
                    meta: meta.clone(),
                })?;
        match (left, right) {
            (_, _) if left_tid == right_tid => Ok(left_tid),
            (Type::Never, _) | (_, Type::Never) => Ok(self.intern(Type::Never)),
            (Type::Pointer(left), Type::Pointer(right)) => {
                match (left.mutability, right.mutability) {
                    (Mutability::Mutable, Mutability::Mutable) => {
                        self.unify_ty_ty(env, left.tid, right.tid, meta)
                            .map_err(|kind| InferenceError {
                                kind,
                                origin: origin.clone(),
                                meta: meta.clone(),
                            })?;
                        let tid = self
                            .normalize_ty(env, left.tid, meta)
                            .map_err(|kind| InferenceError {
                                kind,
                                origin: origin.clone(),
                                meta: meta.clone(),
                            })?
                            .1;
                        Ok(self.pointer(tid, Mutability::Mutable))
                    }
                    (Mutability::Mutable, Mutability::Const) => {
                        self.check_subtype(env, left.tid, right.tid, origin, meta)?;
                        let tid = self
                            .normalize_ty(env, left.tid, meta)
                            .map_err(|kind| InferenceError {
                                kind,
                                origin: origin.clone(),
                                meta: meta.clone(),
                            })?
                            .1;
                        Ok(self.pointer(tid, Mutability::Mutable))
                    }
                    (Mutability::Const, Mutability::Mutable) => {
                        self.check_subtype(env, right.tid, left.tid, origin, meta)?;
                        let tid = self
                            .normalize_ty(env, right.tid, meta)
                            .map_err(|kind| InferenceError {
                                kind,
                                origin: origin.clone(),
                                meta: meta.clone(),
                            })?
                            .1;
                        Ok(self.pointer(tid, Mutability::Mutable))
                    }
                    (Mutability::Const, Mutability::Const) => {
                        let tid = self.meet_ty_ty(env, left.tid, right.tid, origin, meta)?;
                        Ok(self.pointer(tid, Mutability::Const))
                    }
                }
            }
            (Type::Variant(left), Type::Variant(right))
                if left.eid == right.eid && left.variant != right.variant =>
            {
                Err(InferenceError::types_incompatible(
                    left_tid, right_tid, origin, meta,
                ))
            }
            (Type::Variant(left), Type::Variant(right))
                if left.eid == right.eid && left.variant == right.variant =>
            {
                let variances = self.get_enum_def(left.eid).variances.clone();
                let args =
                    self.meet_args(env, &variances, &left.args, &right.args, origin, meta)?;
                Ok(self.intern(Type::Variant(VariantType {
                    eid: left.eid,
                    args,
                    variant: left.variant,
                })))
            }
            (Type::Variant(v), Type::Enum(e)) if v.eid == e.eid => {
                let variances = self.get_enum_def(e.eid).variances.clone();
                let args = self.meet_args(env, &variances, &e.args, &v.args, origin, meta)?;
                Ok(self.intern(Type::Variant(VariantType {
                    eid: v.eid,
                    args,
                    variant: v.variant,
                })))
            }
            (Type::Enum(e), Type::Variant(v)) if v.eid == e.eid => {
                let variances = self.get_enum_def(e.eid).variances.clone();
                let args = self.meet_args(env, &variances, &e.args, &v.args, origin, meta)?;
                Ok(self.intern(Type::Variant(VariantType {
                    eid: v.eid,
                    args,
                    variant: v.variant,
                })))
            }
            (Type::Enum(left), Type::Enum(right)) if left.eid == right.eid => {
                let variances = self.get_enum_def(left.eid).variances.clone();
                let args =
                    self.meet_args(env, &variances, &left.args, &right.args, origin, meta)?;
                Ok(self.intern(Type::Enum(EnumType {
                    eid: left.eid,
                    args,
                })))
            }
            (Type::Struct(left), Type::Struct(right)) if left.sid == right.sid => {
                let variances = self.get_struct_def(left.sid).variances.clone();
                let args =
                    self.meet_args(env, &variances, &left.args, &right.args, origin, meta)?;
                Ok(self.intern(Type::Struct(StructType {
                    sid: left.sid,
                    args,
                })))
            }
            (_, _) => {
                self.unify_ty_ty(env, left_tid, right_tid, meta)
                    .map_err(|kind| InferenceError {
                        kind,
                        origin: origin.clone(),
                        meta: meta.clone(),
                    })?;
                Ok(self
                    .normalize_ty(env, left_tid, meta)
                    .map_err(|kind| InferenceError {
                        kind,
                        origin: origin.clone(),
                        meta: meta.clone(),
                    })?
                    .1)
            }
        }
    }

    fn join_args(
        &mut self,
        env: &Environment,
        variances: &[Variance],
        left: &[TypeId],
        right: &[TypeId],
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Result<Vec<TypeId>, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        assert!(variances.len() == left.len());
        assert!(variances.len() == right.len());

        let mut args = vec![];
        for ((variance, left), right) in variances.iter().zip(left).zip(right) {
            let tid = match variance {
                Variance::Covariant => self.join_ty_ty(env, *left, *right, origin, meta)?,
                Variance::Invariant => {
                    self.unify_ty_ty(env, *left, *right, meta)
                        .map_err(|kind| InferenceError {
                            kind,
                            origin: origin.clone(),
                            meta: meta.clone(),
                        })?;
                    self.normalize_ty(env, *left, meta)
                        .map_err(|kind| InferenceError {
                            kind,
                            origin: origin.clone(),
                            meta: meta.clone(),
                        })?
                        .1
                }
                Variance::Contravariant => self.meet_ty_ty(env, *left, *right, origin, meta)?,
            };

            args.push(tid)
        }
        Ok(args)
    }

    pub fn join_ty_ty(
        &mut self,
        env: &Environment,
        left: TypeId,
        right: TypeId,
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Result<TypeId, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        let (left, left_tid) =
            self.normalize_ty(env, left, meta)
                .map_err(|kind| InferenceError {
                    kind,
                    origin: origin.clone(),
                    meta: meta.clone(),
                })?;
        let (right, right_tid) =
            self.normalize_ty(env, right, meta)
                .map_err(|kind| InferenceError {
                    kind,
                    origin: origin.clone(),
                    meta: meta.clone(),
                })?;
        match (left, right) {
            (_, _) if left_tid == right_tid => Ok(left_tid),
            (Type::Never, _) => Ok(right_tid),
            (_, Type::Never) => Ok(left_tid),
            (Type::Pointer(left), Type::Pointer(right)) => {
                let left_normal = self
                    .normalize_ty(env, left.tid, meta)
                    .map_err(|kind| InferenceError {
                        kind,
                        origin: origin.clone(),
                        meta: meta.clone(),
                    })?
                    .1;
                let right_normal = self
                    .normalize_ty(env, right.tid, meta)
                    .map_err(|kind| InferenceError {
                        kind,
                        origin: origin.clone(),
                        meta: meta.clone(),
                    })?
                    .1;

                if left.mutability == Mutability::Mutable
                    && right.mutability == Mutability::Mutable
                    && left_normal == right_normal
                {
                    Ok(self.pointer(left_normal, Mutability::Mutable))
                } else {
                    let tid = self.join_ty_ty(env, left.tid, right.tid, origin, meta)?;
                    Ok(self.pointer(tid, Mutability::Const))
                }
            }
            (Type::Variant(left), Type::Variant(right))
                if left.eid == right.eid && left.variant != right.variant =>
            {
                //  todo!("Handle variant specific Variances")
                let variances = self.get_enum_def(left.eid).variances.clone();
                let args =
                    self.join_args(env, &variances, &left.args, &right.args, origin, meta)?;
                Ok(self.intern(Type::Enum(EnumType {
                    eid: left.eid,
                    args,
                })))
            }
            (Type::Variant(left), Type::Variant(right))
                if left.eid == right.eid && left.variant == right.variant =>
            {
                let variances = self.get_enum_def(left.eid).variances.clone();
                let args =
                    self.join_args(env, &variances, &left.args, &right.args, origin, meta)?;
                Ok(self.intern(Type::Variant(VariantType {
                    eid: left.eid,
                    args,
                    variant: left.variant,
                })))
            }
            (Type::Variant(v), Type::Enum(e)) if v.eid == e.eid => {
                let variances = self.get_enum_def(e.eid).variances.clone();
                let args = self.join_args(env, &variances, &v.args, &e.args, origin, meta)?;
                Ok(self.intern(Type::Enum(EnumType { eid: v.eid, args })))
            }
            (Type::Enum(e), Type::Variant(v)) if v.eid == e.eid => {
                let variances = self.get_enum_def(e.eid).variances.clone();
                let args = self.join_args(env, &variances, &e.args, &v.args, origin, meta)?;
                Ok(self.intern(Type::Enum(EnumType { eid: v.eid, args })))
            }
            (Type::Enum(left), Type::Enum(right)) if left.eid == right.eid => {
                let variances = self.get_enum_def(left.eid).variances.clone();
                let args =
                    self.join_args(env, &variances, &left.args, &right.args, origin, meta)?;
                Ok(self.intern(Type::Enum(EnumType {
                    eid: left.eid,
                    args,
                })))
            }
            (Type::Struct(left), Type::Struct(right)) if left.sid == right.sid => {
                let variances = self.get_struct_def(left.sid).variances.clone();
                let args =
                    self.join_args(env, &variances, &left.args, &right.args, origin, meta)?;
                Ok(self.intern(Type::Struct(StructType {
                    sid: left.sid,
                    args,
                })))
            }
            (Type::Tuple(left), Type::Tuple(right)) if left.len() == right.len() => {
                if left.len() != right.len() {
                    return Err(InferenceError::types_incompatible(
                        left_tid, right_tid, origin, meta,
                    ));
                }

                let tids = left
                    .into_iter()
                    .zip(right.into_iter())
                    .map(|(left, right)| self.join_ty_ty(env, left, right, origin, meta))
                    .collect::<Result<Vec<_>, _>>()?;

                Ok(self.tuple(tids))
            }
            (_, _) => {
                self.unify_ty_ty(env, left_tid, right_tid, meta)
                    .map_err(|kind| InferenceError {
                        kind,
                        origin: origin.clone(),
                        meta: meta.clone(),
                    })?;
                Ok(self
                    .normalize_ty(env, left_tid, meta)
                    .map_err(|kind| InferenceError {
                        kind,
                        origin: origin.clone(),
                        meta: meta.clone(),
                    })?
                    .1)
            }
        }
    }

    fn check_join(
        &mut self,
        env: &Environment,
        output: TypeId,
        input: &Vec<TypeId>,
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Result<(), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        let (&first, rest) = input
            .split_first()
            .expect("Join should have at least one input");

        let mut joined = first;
        for tid in rest {
            joined = self.join_ty_ty(env, joined, *tid, origin, meta)?;
        }

        self.unify_ty_ty(env, output, joined, meta)
            .map_err(|kind| InferenceError {
                kind,
                origin: origin.clone(),
                meta: meta.clone(),
            })
    }

    fn check_constraints(
        &mut self,
        env: &Environment,
        constraints: &mut Vec<Constraint<M>>,
    ) -> Result<(), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        constraints.sort();
        for c in constraints {
            match &c.constraint {
                ConstraintType::Equal(left, right) => self
                    .unify_ty_ty(env, *left, *right, &c.meta)
                    .map_err(|kind| InferenceError {
                        kind,
                        origin: c.origin.clone(),
                        meta: c.meta.clone(),
                    })?,
                ConstraintType::Subtype { subty, superty } => {
                    self.check_subtype(env, *subty, *superty, &c.origin, &c.meta)?
                }
                ConstraintType::Join { output, inputs } => {
                    self.check_join(env, *output, inputs, &c.origin, &c.meta)?
                }
                ConstraintType::Predicate(pred) => {
                    self.check_predicate(env, pred, &c.origin, &c.meta)?
                }
                ConstraintType::Arity { expected, found } => {
                    if expected.len() != found.len() {
                        return Err(InferenceError::stacks_differ(
                            expected.clone(),
                            found.clone(),
                            &c.origin,
                            &c.meta,
                        ));
                    }
                }
                ConstraintType::HasEnoughArgs { expected, found } => {
                    if expected.len() > found.len() {
                        return Err(InferenceError::not_enough_args(
                            expected.clone(),
                            found.clone(),
                            &c.origin,
                            &c.meta,
                        ));
                    }
                }
            }
        }

        Ok(())
    }

    pub fn type_check<'src, Lookup>(
        &mut self,
        expr: Expr<'src, M>,
        stack: &Stack,
        target: &Stack,
        env: &mut Environment,
        lookup: &Lookup,
    ) -> Result<(Expr<'src, (M, TypeId)>, HashSet<TypeVar>), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
        Lookup: MemberLookup,
    {
        // Determine which types are unbound at the start.

        let (mut unbound, mut stack) = self.substitute(stack);
        // Get constraints from inference
        let (out_expr, mut constraints) = self.infer(expr, &mut stack, env, lookup)?;

        // Add constraint that the target and stack must be equal.
        constraints.extend(Constraint::output_subtyping(
            target,
            &stack,
            &out_expr.meta.0,
        ));

        // Make sure that constraints can be resolved.
        self.check_constraints(&env, &mut constraints)?;

        let (unbound_stack, _) = self.substitute(&stack);
        let (unbound_expr, expr) = self.substitute_expr(out_expr);

        unbound.extend(unbound_stack);
        unbound.extend(unbound_expr);

        Ok((expr, unbound))
    }

    pub fn type_instance(
        &mut self,
        tid: TypeId,
        args: Option<Vec<TypeId>>,
        meta: &M,
    ) -> Result<TypeId, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        match self.get(tid).expect("TypeIds should be resolvable") {
            Type::Struct(s) => {
                let s = if let Some(args) = args {
                    if s.args.len() != args.len() {
                        todo!("Wrong number of type parameters provided: {:?}", meta);
                    }
                    // Just overwrite the args
                    StructType { sid: s.sid, args }
                } else {
                    let s = s.clone();
                    self.freshen_struct(&s, &mut HashMap::new(), meta)
                };
                Ok(self.intern(Type::Struct(s)))
            }
            Type::TypeVar(_)
            | Type::RigidTypeVar(_)
            | Type::Builtin(_)
            | Type::Never
            | Type::Function(_)
            | Type::Pointer(_)
            | Type::Tuple(_)
            | Type::Type(_) => {
                if args.is_some() {
                    todo!("Passed args to type which didn't expect args ");
                }
                Ok(tid)
            }
            Type::Enum(e) => {
                let e = if let Some(args) = args {
                    if e.args.len() != args.len() {
                        todo!("Wrong number of type parameters provided");
                    }
                    // Just overwrite the args
                    EnumType { eid: e.eid, args }
                } else {
                    let e = e.clone();
                    self.freshen_enum(&e, &mut HashMap::new(), meta)
                };

                Ok(self.intern(Type::Enum(e)))
            }
            Type::Variant(variant) => {
                let variant = if let Some(args) = args {
                    if variant.args.len() != args.len() {
                        todo!("Wrong number of type parameters provided");
                    }
                    // Just overwrite the args
                    VariantType {
                        eid: variant.eid,
                        args,
                        variant: variant.variant,
                    }
                } else {
                    let variant = variant.clone();
                    self.freshen_variant(&variant, &mut HashMap::new(), meta)
                };

                Ok(self.intern(Type::Variant(variant)))
            }
            Type::Projection(p) => {
                let p = if let Some(args) = args {
                    if p.args.len() != args.len() {
                        todo!("Wrong number of type parameters provided: {:?}", meta);
                    }
                    // Just overwrite the args
                    Projection {
                        cid: p.cid,
                        args,
                        idx: p.idx,
                    }
                } else {
                    let p = p.clone();
                    self.freshen_projection(&p, &mut HashMap::new(), meta)
                };
                Ok(self.intern(Type::Projection(p)))
            }
        }
    }

    pub fn replace(&mut self, tid: TypeId, replacements: &HashMap<TypeVar, TypeId>) -> TypeId {
        match self.get(tid).unwrap() {
            Type::TypeVar(tyvar) | Type::RigidTypeVar(tyvar) => {
                if let Some(tid) = replacements.get(tyvar) {
                    *tid
                } else {
                    tid
                }
            }
            Type::Builtin(_) => tid,
            Type::Function(func) => {
                let mut func = func.clone();
                func.input = func
                    .input
                    .into_iter()
                    .map(|tid| self.replace(tid, replacements))
                    .collect();
                func.output = func
                    .output
                    .into_iter()
                    .map(|tid| self.replace(tid, replacements))
                    .collect();
                self.function(func)
            }
            Type::Pointer(p) => {
                let p = p.clone();
                let tid = self.replace(p.tid, replacements);
                self.pointer(tid, p.mutability)
            }
            Type::Struct(s) => {
                let s = s.clone();
                let args = s
                    .args
                    .into_iter()
                    .map(|tid| self.replace(tid, replacements))
                    .collect();
                self.intern(Type::Struct(StructType { sid: s.sid, args }))
            }
            Type::Enum(e) => {
                let e = e.clone();
                let args = e
                    .args
                    .into_iter()
                    .map(|tid| self.replace(tid, replacements))
                    .collect();
                self.intern(Type::Enum(EnumType { eid: e.eid, args }))
            }
            Type::Tuple(tids) => {
                let tids = tids
                    .clone()
                    .into_iter()
                    .map(|tid| self.replace(tid, replacements))
                    .collect();
                self.intern(Type::Tuple(tids))
            }
            Type::Variant(v) => {
                let v = v.clone();
                let args = v
                    .args
                    .into_iter()
                    .map(|tid| self.replace(tid, replacements))
                    .collect();
                self.intern(Type::Variant(VariantType {
                    eid: v.eid,
                    args,
                    variant: v.variant,
                }))
            }
            Type::Type(type_id) => {
                let inner = self.replace(*type_id, replacements);
                self.intern(Type::Type(inner))
            }
            Type::Projection(p) => {
                let mut p = p.clone();
                p.args = p
                    .args
                    .into_iter()
                    .map(|tid| self.replace(tid, replacements))
                    .collect();
                self.intern(Type::Projection(p))
            }
            Type::Never => todo!(),
        }
    }

    pub fn get_enum_variant(
        &mut self,
        enum_tid: TypeId,
        variant_id: usize,
        meta: &M,
    ) -> Result<TypeId, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        let variant = match self.get(enum_tid).expect("TypeIds should be resolvable") {
            Type::Enum(enum_type) => {
                let enum_def = &self.enums[enum_type.eid.0];
                if enum_def.variants.len() < variant_id {
                    todo!("Invalid variant id: {:?}", meta);
                }
                VariantType {
                    eid: enum_type.eid,
                    args: enum_type.args.clone(),
                    variant: variant_id,
                }
            }

            Type::TypeVar(_)
            | Type::RigidTypeVar(_)
            | Type::Builtin(_)
            | Type::Function(_)
            | Type::Pointer(_)
            | Type::Struct(_)
            | Type::Variant(_)
            | Type::Type(_)
            | Type::Tuple(_)
            | Type::Never
            | Type::Projection(_) => todo!("{} is not an enum", self.fmt(enum_tid)),
        };

        Ok(self.intern(Type::Variant(variant)))
    }

    pub fn metatype(
        &mut self,
        tid: TypeId,
        args: Option<Vec<TypeId>>,
        meta: &M,
    ) -> Result<TypeId, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        let tid = self.type_instance(tid, args, meta)?;
        Ok(self.intern(Type::Type(tid)))
    }

    pub fn is_free(&self, tid: &TypeId) -> bool {
        match &self.types[tid.0] {
            Type::TypeVar(_) => true,
            Type::RigidTypeVar(_) => true,
            Type::Builtin(BuiltinType::CVarArgs) => true,
            Type::Builtin(_) => false,
            Type::Function(func) => func
                .input
                .iter()
                .chain(func.output.iter())
                .any(|tid| self.is_free(tid)),
            Type::Pointer(p) => self.is_free(&p.tid),
            Type::Struct(s) => s.args.iter().any(|tid| self.is_free(tid)),
            Type::Enum(e) => e.args.iter().any(|tid| self.is_free(tid)),
            Type::Tuple(tids) => tids.iter().any(|tid| self.is_free(tid)),
            Type::Variant(v) => v.args.iter().any(|tid| self.is_free(tid)),
            Type::Type(tid) => self.is_free(tid),
            Type::Projection(_) => todo!(),
            Type::Never => todo!(),
        }
    }

    pub fn assign_vars_from_concrete(
        &mut self,
        env: &Environment,
        scheme: &Scheme,
        tid: TypeId,
        meta: &M,
    ) -> Result<Vec<TypeId>, InferenceErrorKind> {
        let mut fresh = HashMap::new();

        let vars: Vec<_> = scheme
            .vars
            .iter()
            .map(|var| self.freshen_type_var(*var, &mut fresh, meta))
            .collect();
        let scheme_tid = self.freshen_tid(scheme.tid, &mut fresh, meta);

        let tid = self.freshen_tid(tid, &mut HashMap::new(), meta);

        self.unify_ty_ty(env, scheme_tid, tid, meta);

        let mut tids = vec![];
        for tid in vars {
            tids.push(self.normalize_ty(env, tid, meta)?.1);
        }

        Ok(tids)
    }
}
