use std::{
    collections::{HashMap, HashSet},
    eprintln,
    f32::consts::E,
    fmt::Debug,
    format,
    panic::Location,
    todo, unreachable, write,
};

use inference::InferenceStatus;

use crate::typecheck::{
    constraint::Member,
    expr::{self, PlaceProjection},
    typ::typ::{Stack, StackState},
};
use crate::{
    error::HayError,
    typecheck::{
        constraint::{Constraint, ConstraintType},
        environment::{
            Coherence, Environment, InterfaceImpl, QualifiedInterfaceImpl, QualifiedType, Scheme,
            TypeClassId, TypePredicate, Variance,
        },
        expr::{Expr, ExprKind, MatchCase, MatchExpr, MatchPattern, Place, Var},
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
    },
};

use super::constraint::ConstraintOrigin;

pub mod inference;

pub trait MemberLookup {
    fn get_member_idx(&self, sid: StructId, member: &str) -> Option<usize>;
}

pub enum ImplResolution {
    Match(QualifiedInterfaceImpl),
    Unmatched,
    Defer,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemberAccess {
    Direct,
    Indirect,
}

#[derive(Debug, Clone, Copy)]
pub struct MemberInfo {
    pub idx: usize,
    pub tid: TypeId,
    pub access: MemberAccess,
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

    revision: usize,
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
            revision: 0,
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
            Type::TypeVar(type_var) => write!(w, "TV({})", type_var.0),
            Type::RigidTypeVar(type_var) => write!(w, "RV({})", type_var.0),
            Type::Builtin(builtin_type) => match builtin_type {
                BuiltinType::U8 => write!(w, "u8"),
                BuiltinType::U64 => write!(w, "u64"),
                BuiltinType::Bool => write!(w, "bool"),
                BuiltinType::Char => write!(w, "char"),
                BuiltinType::CVarArgs => write!(w, "..."),
            },
            Type::Function(function_type) => {
                let mut s = String::from("fn (");
                for tid in &function_type.input {
                    s += " ";
                    self.pretty_print(*tid, &mut s)?;
                }
                s += " ) -> ";

                match &function_type.output {
                    StackState::Reachable(output) => {
                        s += "[";
                        for tid in output {
                            s += " ";
                            self.pretty_print(*tid, &mut s)?;
                        }
                        s += "]"
                    }
                    StackState::Unreachable => s += "!",
                }
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
            ConstraintType::Unreachable(stack) => format!("Expect an unreachable state: {stack:?}"),
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
            ConstraintType::HasMember {
                base_tid,
                member,
                member_tid,
            } => match member {
                Member::Named(member) => {
                    format!(
                        "{} has member `{}` with type {}",
                        self.fmt(*base_tid),
                        member,
                        self.fmt(*member_tid)
                    )
                }
                Member::Idx(member) => {
                    format!(
                        "{} has member `{}` with type {}",
                        self.fmt(*base_tid),
                        member,
                        self.fmt(*member_tid)
                    )
                }
            },
        }
    }

    pub fn get_variance(&self, tid: TypeId, tyvar: TypeVar) -> Option<Variance> {
        match self.types.get(tid.0).expect("TypeIds should be resolvable") {
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

                let output_variance = match &f.output {
                    StackState::Reachable(output) => output.iter().fold(None, |result, tid| {
                        let variance = self
                            .get_variance(*tid, tyvar)
                            .map(|v| Variance::Covariant.compose(v));
                        Variance::combine(result, variance)
                    }),
                    StackState::Unreachable => None,
                };

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
            Type::Builtin(_) | Type::RigidTypeVar(_) => (HashSet::new(), tid),
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
                let (unbound_input, input) = self.substitute_stack(&func.input);
                let (unbound_output, output) = self.substitute_stack_state(&func.output);

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
                let (unbound, args) = self.substitute_stack(&s.args);
                let s = StructType { sid: s.sid, args };
                let tid = self.intern(Type::Struct(s));
                (unbound, tid)
            }
            Type::Enum(e) => {
                let e = e.clone();
                let (unbound, args) = self.substitute_stack(&e.args);
                let e = EnumType { eid: e.eid, args };
                let tid = self.intern(Type::Enum(e));
                (unbound, tid)
            }
            Type::Variant(v) => {
                let v = v.clone();
                let (unbound, args) = self.substitute_stack(&v.args);
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
                let (unbound, tids) = self.substitute_stack(&tids);
                let tid = self.tuple(tids);
                (unbound, tid)
            }
            Type::Projection(p) => {
                let p = p.clone();
                let (unbound, args) = self.substitute_stack(&p.args);
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
    fn substitute_stack(&mut self, stack: &Stack) -> (HashSet<TypeVar>, Stack) {
        let mut unbound = HashSet::new();
        let mut stk = vec![];
        for tid in stack {
            let (ub, tid) = self.substitute_one(*tid);
            unbound.extend(ub);
            stk.push(tid);
        }

        (unbound, stk)
    }

    fn substitute_stack_state(&mut self, stack: &StackState) -> (HashSet<TypeVar>, StackState) {
        match stack {
            StackState::Reachable(stack) => {
                let (unbound, stack) = self.substitute_stack(stack);
                (unbound, stack.into())
            }
            StackState::Unreachable => (HashSet::new(), StackState::Unreachable),
        }
    }

    pub fn substitute_predicate(
        &mut self,
        predicate: &TypePredicate,
    ) -> (HashSet<TypeVar>, TypePredicate) {
        let (unbound, args) = self.substitute_stack(&predicate.args);
        (
            unbound,
            TypePredicate {
                cid: predicate.cid,
                args,
            },
        )
    }

    /// Substitute all types referenced in an Expression into common form.
    fn substitute_expr<'src, Lookup>(
        &mut self,
        expr: Expr<'src, (M, TypeId)>,
        lookup: &Lookup,
    ) -> (HashSet<TypeVar>, Expr<'src, (M, TypeId)>)
    where
        M: Debug,
        Lookup: MemberLookup,
    {
        let (mut unbound, expr_tid) = self.substitute_one(expr.meta.1);

        match expr.expr {
            ExprKind::U8(_)
            | ExprKind::U64(_)
            | ExprKind::Bool(_)
            | ExprKind::String(_)
            | ExprKind::CString(_)
            | ExprKind::Char(_)
            | ExprKind::Bind(_)
            | ExprKind::Call
            | ExprKind::UncheckedCast
            | ExprKind::Pack
            | ExprKind::Unpack
            | ExprKind::SizeOf
            | ExprKind::Read
            | ExprKind::Write
            | ExprKind::Add(_)
            | ExprKind::Sub(_)
            | ExprKind::Mul(_)
            | ExprKind::LessThan(_)
            | ExprKind::Equal(_) => (
                unbound,
                Expr {
                    expr: expr.expr,
                    meta: (expr.meta.0, expr_tid),
                },
            ),
            ExprKind::Place(mut place) => {
                let Type::Function(FunctionType { input, output }) =
                    self.get(expr_tid).unwrap().clone()
                else {
                    unreachable!()
                };
                assert!(input.len() == 1);
                assert!(output.is_reachable());
                assert!(output.reachable().len() == 1);

                self.substitute_place_projections(
                    input[0],
                    &mut place.projections,
                    lookup,
                    &expr.meta.0,
                );

                (
                    unbound,
                    Expr {
                        expr: ExprKind::Place(place),
                        meta: (expr.meta.0, output.reachable()[0]),
                    },
                )
            }
            ExprKind::Ref {
                mut place,
                mutability,
            } => {
                let Type::Function(FunctionType { input, output }) =
                    self.get(expr_tid).unwrap().clone()
                else {
                    unreachable!()
                };
                assert!(input.len() == 1);
                assert!(output.is_reachable());
                assert!(output.reachable().len() == 1);

                self.substitute_place_projections(
                    input[0],
                    &mut place.projections,
                    lookup,
                    &expr.meta.0,
                );

                (
                    unbound,
                    Expr {
                        expr: ExprKind::Ref { place, mutability },
                        meta: (expr.meta.0, output.reachable()[0]),
                    },
                )
            }
            ExprKind::Projection(mut proj) => {
                let Type::Function(FunctionType { input, output }) =
                    self.get(expr_tid).unwrap().clone()
                else {
                    unreachable!()
                };

                assert!(input.len() == 1);
                assert!(output.is_reachable());
                assert!(output.reachable().len() == 1);

                self.substitute_place_projections(input[0], &mut proj, lookup, &expr.meta.0);

                (
                    unbound,
                    Expr {
                        expr: ExprKind::Projection(proj),
                        meta: (expr.meta.0, expr_tid),
                    },
                )
            }
            ExprKind::Block(exprs) => {
                let exprs = exprs.into_iter().fold(vec![], |mut exprs, e| {
                    let (ub, e) = self.substitute_expr(e, lookup);
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
                        let (case_unbound, expr) = self.substitute_expr(*case.expr, lookup);
                        unbound.extend(case_unbound);
                        MatchCase::new(case.pattern, expr)
                    })
                    .collect();
                let finally = if let Some(expr) = m.finally {
                    let (finally_unbound, expr) = self.substitute_expr(*expr, lookup);
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
            ExprKind::While { condition, body } => {
                let condition = condition
                    .into_iter()
                    .map(|e| {
                        let (ub, expr) = self.substitute_expr(e, lookup);
                        unbound.extend(ub);
                        expr
                    })
                    .collect();

                let (ub, body) = self.substitute_expr(*body, lookup);
                unbound.extend(ub);
                (
                    unbound,
                    Expr::while_(condition, body, (expr.meta.0, expr_tid)),
                )
            }
            ExprKind::If(iff) => {
                let (then_unbound, then) = self.substitute_expr(*iff.then, lookup);
                let (otherwise_unbound, otherwise) = self.substitute_expr(*iff.otherwise, lookup);

                unbound.extend(then_unbound);
                unbound.extend(otherwise_unbound);

                (unbound, Expr::iff(then, otherwise, (expr.meta.0, expr_tid)))
            }
            ExprKind::Tuple(exprs) => {
                let exprs = exprs.into_iter().fold(vec![], |mut acc, e| {
                    let (ub, e) = self.substitute_expr(e, lookup);
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

    fn freshen_vars(&mut self, vars: &[TypeVar], meta: &M) -> HashMap<TypeVar, TypeId> {
        let mut fresh = HashMap::new();

        for var in vars {
            self.freshen_type_var(*var, &mut fresh, meta);
        }

        fresh
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

    fn collect_type_vars(&self, tid: TypeId, vars: &mut HashSet<TypeVar>) {
        match self.get(tid).unwrap() {
            Type::TypeVar(type_var) => {
                vars.insert(*type_var);
            }
            Type::RigidTypeVar(type_var) => (),
            Type::Builtin(builtin_type) => (),
            Type::Function(func) => {
                for tid in func.input.iter().chain(match &func.output {
                    StackState::Reachable(output) => output.iter(),
                    StackState::Unreachable => [].iter(),
                }) {
                    self.collect_type_vars(*tid, vars);
                }
            }
            Type::Pointer(p) => {
                self.collect_type_vars(p.tid, vars);
            }
            Type::Struct(StructType { args: tids, .. })
            | Type::Enum(EnumType { args: tids, .. })
            | Type::Tuple(tids)
            | Type::Variant(VariantType { args: tids, .. })
            | Type::Projection(Projection { args: tids, .. }) => {
                for tid in tids {
                    self.collect_type_vars(*tid, vars);
                }
            }
            Type::Type(tid) => self.collect_type_vars(*tid, vars),
        }
    }

    pub fn freshen_all(&mut self, tid: TypeId, meta: &M) -> TypeId {
        let mut vars = HashSet::new();

        self.collect_type_vars(tid, &mut vars);

        let mut fresh = self.freshen_vars(&vars.into_iter().collect::<Vec<_>>(), meta);

        self.freshen_tid(tid, &mut fresh, meta)
    }

    pub fn freshen_tid(
        &mut self,
        tid: TypeId,
        fresh: &HashMap<TypeVar, TypeId>,
        meta: &M,
    ) -> TypeId {
        match self.get(tid).expect("All TypeIds should be resolvable") {
            Type::Builtin(_) | Type::RigidTypeVar(_) => tid,
            Type::TypeVar(var) => *fresh.get(var).unwrap_or(&tid),
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
    pub fn freshen(&mut self, scheme: &Scheme, meta: &M) -> QualifiedType {
        let mut fresh = self.freshen_vars(&scheme.vars, meta);
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
        fresh: &HashMap<TypeVar, TypeId>,
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
        fresh: &HashMap<TypeVar, TypeId>,
        meta: &M,
    ) -> FunctionType {
        // Create fresh vars for all the types the function is paramaterized over
        let input = self.freshen_many_tids(&func.input, fresh, meta);
        let output = match &func.output {
            StackState::Reachable(output) => {
                StackState::Reachable(self.freshen_many_tids(output, fresh, meta))
            }
            StackState::Unreachable => StackState::Unreachable,
        };
        FunctionType { input, output }
    }

    fn infer_fn_call(
        &mut self,
        func: &FunctionType,
        predicates: Vec<TypePredicate>,
        stack: &mut StackState,
        meta: &M,
    ) -> Vec<Constraint<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        assert!(stack.is_reachable());

        // We're going to defer the check for enough arguments to get as
        // much typing information as possible. So if we don't have enough
        // args, just consume the stack. The error will be reported during
        // constraint checking.
        let argument_constraint = Constraint::new(
            ConstraintType::HasEnoughArgs {
                expected: func.input.clone(),
                found: stack.reachable().clone(),
            },
            ConstraintOrigin::FunctionCall {
                input: stack.reachable().clone(),
                expected: func.input.clone(),
            },
            meta,
        );
        let split = if stack.reachable().len() < func.input.len() {
            stack.split_off(0)
        } else {
            stack.split_off(stack.reachable().len() - func.input.len())
        }
        .unwrap();
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

        stack.append_output(func.output.clone());

        constraints.collect()
    }

    fn substitute_place_projection<'src, Lookup>(
        &mut self,
        tid: TypeId,
        projection: &mut PlaceProjection<'src>,
        lookup: &Lookup,
        meta: &M,
    ) -> TypeId
    where
        Lookup: MemberLookup,
    {
        let (proj, tid) = match projection {
            PlaceProjection::Named(member) => {
                match self.get_member_idx(tid, member, lookup, meta).unwrap() {
                    Some(info) => (PlaceProjection::Indexed(info.idx, info.access), info.tid),
                    None => unreachable!(),
                }
            }
            PlaceProjection::Indexed(idx, access) => return tid,
        };

        *projection = proj;
        tid
    }

    fn substitute_place_projections<'src, Lookup>(
        &mut self,
        tid: TypeId,
        projections: &mut [PlaceProjection<'src>],
        lookup: &Lookup,
        meta: &M,
    ) where
        Lookup: MemberLookup,
    {
        let mut tid = tid;
        for proj in projections {
            tid = self.substitute_place_projection(tid, proj, lookup, meta);
        }
    }

    fn infer_place_projection<'src>(
        &mut self,
        tid: TypeId,
        projection: &PlaceProjection<'src>,
        meta: &M,
    ) -> (TypeId, Constraint<M>) {
        let tyvar = self.fresh_type_var();
        let member_tid = self.intern(Type::TypeVar(tyvar));

        match projection {
            PlaceProjection::Named(member) => {
                let c = Constraint::new(
                    ConstraintType::HasMember {
                        base_tid: tid,
                        member: Member::Named(String::from(*member)),
                        member_tid,
                    },
                    ConstraintOrigin::PlaceProjection,
                    meta,
                );

                (member_tid, c)
            }
            PlaceProjection::Indexed(idx, _) => {
                let c = Constraint::new(
                    ConstraintType::HasMember {
                        base_tid: tid,
                        member: Member::Idx(*idx),
                        member_tid,
                    },
                    ConstraintOrigin::PlaceProjection,
                    meta,
                );

                (member_tid, c)
            }
        }
    }

    fn infer_place_projections<'src>(
        &mut self,
        tid: TypeId,
        projections: &[PlaceProjection<'src>],
        meta: &M,
    ) -> (TypeId, Vec<Constraint<M>>) {
        let mut tid = tid;
        let mut constraints = vec![];

        for p in projections {
            let (next_tid, c) = self.infer_place_projection(tid, p, meta);
            tid = next_tid;
            constraints.push(c);
        }

        (tid, constraints)
    }

    fn infer_place<'src>(
        &mut self,
        place: Place<'src>,
        meta: M,
        stack: &mut StackState,
        env: &mut Environment,
    ) -> Result<(Expr<'src, (M, TypeId)>, Vec<Constraint<M>>), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        let scheme = env.get_var(&place.base).expect("Vars should be resolvable");
        let inst = self.freshen(scheme, &meta);

        let (tid, constraints) = if let Some(func) = self.get_func(&inst.tid).cloned() {
            if !place.projections.is_empty() {
                todo!()
            }

            let tid = self.function(FunctionType {
                input: vec![inst.tid],
                output: vec![inst.tid].into(),
            });

            (
                tid,
                self.infer_fn_call(&func, inst.predicates, stack, &meta),
            )
        } else {
            // If an identifier _isn't_ a funciton, then add the value to the stack.
            let tid = inst.tid;
            let mut constraints: Vec<Constraint<M>> = inst
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

            let (tid, proj_constrs) = self.infer_place_projections(tid, &place.projections, &meta);
            stack.push(tid);

            constraints.extend(proj_constrs);

            let tid = self.function(FunctionType {
                input: vec![inst.tid],
                output: vec![tid].into(),
            });
            (tid, constraints)
        };

        let e = Expr {
            expr: ExprKind::Place(place),
            meta: (meta, tid),
        };

        Ok((e, constraints))
    }

    fn infer_ref<'src>(
        &mut self,
        place: Place<'src>,
        mutability: Mutability,
        meta: M,
        stack: &mut StackState,
        env: &mut Environment,
    ) -> Result<(Expr<'src, (M, TypeId)>, Vec<Constraint<M>>), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        let scheme = env.get_var(&place.base).expect("Vars should be resolvable");
        let inst = self.freshen(scheme, &meta);
        let (tid, constraints) = self.infer_place_projections(inst.tid, &place.projections, &meta);

        let tid = if let Some(func) = self.get_func(&inst.tid).cloned() {
            if mutability != Mutability::Const {
                return Err(InferenceError::mutable_ref_to_fn(
                    place.base,
                    &ConstraintOrigin::TakeRef {
                        var: place.base,
                        mutability,
                    },
                    &meta,
                ));
            }
            self.function(func)
        } else {
            self.pointer(tid, mutability)
        };

        stack.push(tid);
        let e_tid = self.function(FunctionType {
            input: vec![inst.tid],
            output: vec![tid].into(),
        });
        let e = Expr::take_ref(place, mutability, (meta, e_tid));

        Ok((e, constraints))
    }

    fn infer_projection<'src>(
        &mut self,
        projections: Vec<PlaceProjection<'src>>,
        meta: M,
        stack: &mut StackState,
    ) -> Result<(Expr<'src, (M, TypeId)>, Vec<Constraint<M>>), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        assert!(stack.is_reachable());

        if stack.reachable().is_empty() {
            todo!()
        }

        let stack_tid = stack.pop().unwrap();

        let (tid, constraints) = self.infer_place_projections(stack_tid, &projections, &meta);

        stack.push(tid);
        let e_tid = self.function(FunctionType {
            input: vec![stack_tid],
            output: vec![tid].into(),
        });
        let e = Expr {
            expr: ExprKind::Projection(projections),
            meta: (meta, e_tid),
        };

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
            output: vec![into_tid].into(),
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
            Type::RigidTypeVar(_) | Type::Builtin(_) => tid,
            Type::Function(function_type) => {
                let input = self.instantiate_tids(&function_type.input, bindings);
                let output = match &function_type.output {
                    StackState::Reachable(output) => {
                        StackState::Reachable(self.instantiate_tids(output, bindings))
                    }
                    StackState::Unreachable => StackState::Unreachable,
                };
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
                output: vec![tid].into(),
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
                output: vec![tid].into(),
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
                output: vec![tid].into(),
            }),
            Type::Enum(_)
            | Type::TypeVar(_)
            | Type::RigidTypeVar(_)
            | Type::Builtin(_)
            | Type::Function(_)
            | Type::Pointer(_)
            | Type::Type(_)
            | Type::Projection(_) => todo!("Can't pack into a {}", self.fmt(tid)),
        }
    }

    fn generate_unpack_function(&mut self, tid: TypeId) -> Result<FunctionType, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        match self.get(tid).expect("TypeIds should be resolvable") {
            Type::Struct(s) => {
                let s = s.clone();
                let members = self.get_struct_members(s);
                Ok(FunctionType {
                    input: vec![tid],
                    output: members.into(),
                })
            }
            Type::Variant(_) => todo!(),
            Type::Tuple(tids) => Ok(FunctionType {
                input: vec![tid],
                output: tids.clone().into(),
            }),
            Type::Enum(_)
            | Type::TypeVar(_)
            | Type::RigidTypeVar(_)
            | Type::Builtin(_)
            | Type::Function(_)
            | Type::Pointer(_)
            | Type::Type(_)
            | Type::Projection(_) => todo!("Can't pack into a {}", self.fmt(tid)),
        }
    }

    fn get_direct_member_idx<'src, Lookup>(
        &mut self,
        tid: TypeId,
        member: &str,
        lookup: &Lookup,
        meta: &M,
    ) -> Result<Option<MemberInfo>, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
        Lookup: MemberLookup,
    {
        match self.get(tid).unwrap() {
            Type::TypeVar(_) => Ok(None),
            Type::Struct(s) => {
                let s = s.clone();
                let Some(member_idx) = lookup.get_member_idx(s.sid, member) else {
                    return Err(InferenceError::invalid_struct_member(
                        member,
                        &ConstraintOrigin::NamedMemberAccess(tid),
                        meta,
                    ));
                };

                let def = self.get_struct_def(s.sid);
                assert!(def.vars.len() == s.args.len());

                let new_tid = self.replace(
                    def.members[member_idx],
                    &HashMap::from_iter(def.vars.clone().into_iter().zip(s.args.into_iter())),
                );

                let info = MemberInfo {
                    idx: member_idx,
                    tid: new_tid,
                    access: MemberAccess::Direct,
                };

                Ok(Some(info))
            }
            Type::Tuple(_) => todo!("Add support for get tuple member"),
            _ => todo!("Can't get member for a non-struct type"),
        }
    }

    fn get_member_idx<'src, Lookup>(
        &mut self,
        tid: TypeId,
        member: &str,
        lookup: &Lookup,
        meta: &M,
    ) -> Result<Option<MemberInfo>, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
        Lookup: MemberLookup,
    {
        match self.get(tid).unwrap() {
            Type::Pointer(p) => match self.get_direct_member_idx(p.tid, member, lookup, meta)? {
                Some(info) => Ok(Some(MemberInfo {
                    idx: info.idx,
                    tid: info.tid,
                    access: MemberAccess::Indirect,
                })),
                None => Ok(None),
            },
            _ => self.get_direct_member_idx(tid, member, lookup, meta),
        }
    }

    fn get_direct_member_from_idx<'src, Lookup>(
        &mut self,
        tid: TypeId,
        idx: usize,
        lookup: &Lookup,
        meta: &M,
    ) -> Result<Option<MemberInfo>, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
        Lookup: MemberLookup,
    {
        match self.get(tid).unwrap() {
            Type::TypeVar(_) => Ok(None),
            Type::Struct(s) => {
                let s = s.clone();

                let def = self.get_struct_def(s.sid);
                assert!(def.vars.len() == s.args.len());

                if idx >= def.members.len() {
                    todo!()
                }

                let new_tid = self.replace(
                    def.members[idx],
                    &HashMap::from_iter(def.vars.clone().into_iter().zip(s.args.into_iter())),
                );

                let info = MemberInfo {
                    idx: idx,
                    tid: new_tid,
                    access: MemberAccess::Direct,
                };

                Ok(Some(info))
            }
            Type::Tuple(tids) => {
                if idx >= tids.len() {
                    todo!()
                };

                let info = MemberInfo {
                    idx,
                    tid: tids[idx],
                    access: MemberAccess::Direct,
                };
                Ok(Some(info))
            }
            _ => todo!("Can't get member for a non-struct type"),
        }
    }

    fn get_member_from_idx<'src, Lookup>(
        &mut self,
        tid: TypeId,
        idx: usize,
        lookup: &Lookup,
        meta: &M,
    ) -> Result<Option<MemberInfo>, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
        Lookup: MemberLookup,
    {
        match self.get(tid).unwrap() {
            Type::Pointer(p) => match self.get_direct_member_from_idx(p.tid, idx, lookup, meta)? {
                Some(info) => Ok(Some(MemberInfo {
                    idx: info.idx,
                    tid: info.tid,
                    access: MemberAccess::Indirect,
                })),
                None => Ok(None),
            },
            _ => self.get_direct_member_from_idx(tid, idx, lookup, meta),
        }
    }

    fn join_stacks(
        &mut self,
        stacks: &[StackState],
        constraints: &mut Vec<Constraint<M>>,
        meta: &M,
    ) -> Result<StackState, InferenceError<M>> {
        let reachable: Vec<_> = stacks
            .iter()
            .filter_map(|stack| match stack {
                StackState::Reachable(stack) => Some(stack),
                StackState::Unreachable => None,
            })
            .collect();

        let len = match reachable.as_slice() {
            // All branches diverge.
            [] => return Ok(StackState::Unreachable),

            // Only one branch returns.
            [stack] => {
                return Ok(StackState::Reachable((*stack).clone()));
            }

            [first, ..] => first.len(),
        };

        if !reachable.iter().all(|stack| stack.len() == len) {
            let fresh_stack = reachable.first().unwrap();
            let fresh_stack = fresh_stack.into_iter().map(|tid| *tid).collect();
            
            return Err(
                InferenceError {
                    kind: InferenceErrorKind::JoinArity,
                    origin: ConstraintOrigin::Branching { 
                        fresh_stack, 
                        stacks: stacks.to_vec() 
                    },
                    meta: meta.clone(),
                }
            );
        }

        let fresh_stack: Stack = (0..len)
            .map(|_| {
                let tyvar = self.fresh_type_var();
                self.intern(Type::TypeVar(tyvar))
            })
            .collect();

        let output = StackState::Reachable(fresh_stack.clone());

        for (i, tid) in fresh_stack.into_iter().enumerate() {
            let inputs = reachable.iter().map(|stack| stack[i]).collect();

            constraints.push(Constraint::new(
                ConstraintType::Join {
                    output: tid,
                    inputs,
                },
                ConstraintOrigin::Branching {
                    fresh_stack: output.reachable().clone(),
                    stacks: stacks.to_vec(),
                },
                meta,
            ));
        }

        Ok(output)
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
        stack: &mut StackState,
        env: &mut Environment,
        lookup: &Lookup,
    ) -> Result<(Expr<'src, (M, TypeId)>, Vec<Constraint<M>>), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
        Lookup: MemberLookup,
    {
        if !stack.is_reachable() {
            todo!("Unreachable expression")
        }

        match expr.expr {
            ExprKind::U8(val) => {
                let u8_tid = self.intern(Type::u8());
                let func = FunctionType {
                    input: vec![],
                    output: vec![u8_tid].into(),
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::u8(val, (expr.meta.clone(), self.function(func)));
                Ok((e, constraints))
            }
            ExprKind::U64(val) => {
                let u8_tid = self.intern(Type::u64());
                let func = FunctionType {
                    input: vec![],
                    output: vec![u8_tid].into(),
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::u64(val, (expr.meta.clone(), self.function(func)));
                Ok((e, constraints))
            }
            ExprKind::Bool(val) => {
                let bool_tid = self.intern(Type::bool());
                let func = FunctionType {
                    input: vec![],
                    output: vec![bool_tid].into(),
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::bool(val, (expr.meta, self.function(func)));
                Ok((e, constraints))
            }
            ExprKind::String(s) => {
                let str_tid = self.str_tid.expect("Str tid must be provided");
                let func = FunctionType {
                    input: vec![],
                    output: vec![str_tid].into(),
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::string(s, (expr.meta, self.function(func)));
                Ok((e, constraints))
            }
            ExprKind::CString(s) => {
                let char_tid = self.intern(Type::char());
                let char_ptr = self.pointer(char_tid, Mutability::Const);
                let func = FunctionType {
                    input: vec![],
                    output: vec![char_ptr].into(),
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::cstring(s, (expr.meta, self.function(func)));
                Ok((e, constraints))
            }
            ExprKind::Char(val) => {
                let char_tid = self.intern(Type::char());
                let func = FunctionType {
                    input: vec![],
                    output: vec![char_tid].into(),
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::char(val, (expr.meta.clone(), self.function(func)));
                Ok((e, constraints))
            }
            ExprKind::Bind(vars) => {
                if stack.reachable().len() < vars.len() {
                    todo!("Too few elements to bind")
                }

                let popped = stack
                    .split_off(stack.reachable().len() - vars.len())
                    .unwrap();
                popped
                    .iter()
                    .zip(vars.iter())
                    .for_each(|(tid, var)| assert_eq!(env.bind(*var, *tid), None));

                let tid = self.function(FunctionType {
                    input: popped,
                    output: vec![].into(),
                });
                let e = Expr::bind(vars, (expr.meta, tid));
                Ok((e, vec![]))
            }
            ExprKind::Place(place) => self.infer_place(place, expr.meta, stack, env),
            ExprKind::Ref { place, mutability } => {
                self.infer_ref(place, mutability, expr.meta, stack, env)
            }
            ExprKind::Projection(proj) => self.infer_projection(proj, expr.meta, stack),
            ExprKind::Block(exprs) => {
                let input = stack.reachable().clone();

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
                if stack.reachable().is_empty() {
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
                    output: vec![tid].into(),
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
                if stack.reachable().is_empty() {
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
            ExprKind::Unpack => {
                if stack.reachable().is_empty() {
                    todo!("Expected the product type on top of stack");
                }

                let tid = *stack.reachable().last().unwrap();
                let unpack_fn = self.generate_unpack_function(tid)?;
                let constraints = self.infer_fn_call(&unpack_fn, vec![], stack, &expr.meta);

                let expr_tid = self.function(unpack_fn);
                let e = Expr::unpack((expr.meta, expr_tid));
                Ok((e, constraints))
            }
            ExprKind::SizeOf => {
                let u64_tid = self.intern(Type::u64());

                let tyvar = self.fresh_type_var();
                let t_tid = self.intern(Type::TypeVar(tyvar));
                let ty_t_tid = self
                    .metatype(env, t_tid, None, &expr.meta)
                    .map_err(|kind| todo!())?;

                let func = FunctionType {
                    input: vec![ty_t_tid],
                    output: vec![u64_tid].into(),
                };

                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let expr_tid = self.function(func);
                let e = Expr::sizeof((expr.meta, expr_tid));
                Ok((e, constraints))
            }
            ExprKind::Match(m) => {
                let input = stack.reachable().clone();

                assert!(stack.is_reachable());
                // pop an enum off of the stack.
                if stack.reachable().is_empty() {
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

                let fresh_stack = self
                    .join_stacks(&stacks, &mut constraints, &expr.meta)
                    .map_err(|_| todo!())?;
                let expr_tid = self.function(FunctionType {
                    input,
                    output: fresh_stack.clone(),
                });
                let e = Expr::match_(MatchExpr::new(cases, finally), (expr.meta, expr_tid));

                *stack = fresh_stack;

                Ok((e, constraints))
            }
            ExprKind::While { condition, body } => {
                let stack_before = stack.clone();
                let mut constraints = vec![];
                let mut cond = vec![];
                {
                    let mut env = env.clone();
                    for e in condition {
                        let (e, constrs) = self.infer(e, stack, &mut env, lookup)?;
                        cond.push(e);
                        constraints.extend(constrs);
                    }
                }

                if !stack.is_reachable() {
                    *stack = StackState::Unreachable;
                    let tid = self.function(FunctionType {
                        input: stack_before.reachable().clone(),
                        output: StackState::Unreachable,
                    });

                    let expr = Expr::block(cond, (expr.meta, tid));
                    return Ok((expr, constraints));
                }

                let Some(tid) = stack.pop() else {
                    todo!("Need a condition!");
                };

                let bool_tid = self.intern(Type::bool());

                constraints.push(Constraint::new(
                    ConstraintType::Equal(tid, bool_tid),
                    ConstraintOrigin::WhileCondition,
                    &expr.meta,
                ));

                let condition_output = stack.clone();

                let (body, constrs) = self.infer(*body, stack, env, lookup)?;

                constraints.extend(constrs);

                let fresh_stack = self
                    .join_stacks(
                        &[stack_before.clone(), condition_output.clone(), stack.clone()],
                        &mut constraints,
                        &expr.meta,
                    )?;

                let expr_tid = self.function(FunctionType {
                    input: stack_before.reachable().clone(),
                    output: fresh_stack.clone(),
                });
                let e = Expr::while_(cond, body, (expr.meta, expr_tid));

                *stack = fresh_stack;

                Ok((e, constraints))
            }
            ExprKind::If(iff) => {
                let input = stack.reachable().clone();
                // pop an enum off of the stack.
                if input.is_empty() {
                    todo!("Error: Expected an bool")
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

                let fresh_stack = self
                    .join_stacks(&[then_stack, otherwise_stack], &mut constraints, &expr.meta)?;

                let expr_tid = self.function(FunctionType {
                    input,
                    output: fresh_stack.clone(),
                });
                let e = Expr::iff(then, otherwise, (expr.meta, expr_tid));

                *stack = fresh_stack;

                Ok((e, constraints))
            }
            ExprKind::Tuple(exprs) => {
                let mut inner_stack = StackState::Reachable(vec![]);

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

                let output = match inner_stack {
                    StackState::Reachable(inner_stack) => {
                        let tuple_tid = self.tuple(inner_stack);

                        stack.push(tuple_tid);

                        vec![tuple_tid].into()
                    }
                    StackState::Unreachable => {
                        *stack = StackState::Unreachable;
                        StackState::Unreachable
                    }
                };

                let tid = self.function(FunctionType {
                    input: vec![],
                    output,
                });

                let e = Expr::tuple(exprs, (expr.meta, tid));

                Ok((e, constraints))
            }
            ExprKind::Read => {
                let tyvar = self.fresh_type_var();
                let t_tid = self.intern(Type::TypeVar(tyvar));
                let t_ptr_tid = self.pointer(t_tid, Mutability::Const);

                let func = FunctionType {
                    input: vec![t_ptr_tid],
                    output: vec![t_tid].into(),
                };

                let constriants = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let expr_tid = self.function(func);
                let e = Expr::read((expr.meta, expr_tid));
                Ok((e, constriants))
            }
            ExprKind::Write => {
                let tyvar = self.fresh_type_var();
                let t_tid = self.intern(Type::TypeVar(tyvar));
                let t_ptr_tid = self.pointer(t_tid, Mutability::Mutable);

                let func = FunctionType {
                    input: vec![t_tid, t_ptr_tid],
                    output: vec![].into(),
                };

                let constriants = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let expr_tid = self.function(func);
                let e = Expr::write((expr.meta, expr_tid));
                Ok((e, constriants))
            }
            ExprKind::Add(tid) => {
                let func = match self.get(tid) {
                    Some(Type::Function(func)) => func.clone(),
                    _ => unreachable!(),
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::add(tid, (expr.meta.clone(), self.function(func)));
                Ok((e, constraints))
            }
            ExprKind::Sub(tid) => {
                let func = FunctionType {
                    input: vec![tid, tid],
                    output: vec![tid].into(),
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::sub(tid, (expr.meta.clone(), self.function(func)));
                Ok((e, constraints))
            }
            ExprKind::Mul(tid) => {
                let func = FunctionType {
                    input: vec![tid, tid],
                    output: vec![tid].into(),
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::mul(tid, (expr.meta.clone(), self.function(func)));
                Ok((e, constraints))
            }
            ExprKind::Equal(tid) => {
                let bool_tid = self.intern(Type::bool());
                let func = FunctionType {
                    input: vec![tid, tid],
                    output: vec![bool_tid].into(),
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::eq(tid, (expr.meta.clone(), self.function(func)));
                Ok((e, constraints))
            }
            ExprKind::LessThan(tid) => {
                let bool_tid = self.intern(Type::bool());
                let func = FunctionType {
                    input: vec![tid, tid],
                    output: vec![bool_tid].into(),
                };
                let constraints = self.infer_fn_call(&func, vec![], stack, &expr.meta);
                let e = Expr::lt(tid, (expr.meta.clone(), self.function(func)));
                Ok((e, constraints))
            }
        }
    }

    pub fn normalize_ty(
        &mut self,
        env: &Environment,
        tid: TypeId,
        meta: &M,
    ) -> Result<(Type, TypeId), InferenceErrorKind> {
        match self.get(tid).expect("Type Ids should be resolvable") {
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
                let output = match func.output {
                    StackState::Reachable(output) => StackState::Reachable(
                        output
                            .iter()
                            .map(|tid| self.normalize_ty(env, *tid, meta).map(|(_, tid)| tid))
                            .collect::<Result<_, _>>()?,
                    ),
                    StackState::Unreachable => StackState::Unreachable,
                };

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
                if let ImplResolution::Match(QualifiedInterfaceImpl {
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

    fn unify_typevar(
        &mut self,
        var: TypeVar,
        tid: TypeId,
    ) -> Result<InferenceStatus, InferenceErrorKind>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        self.lookup
            .assign_key(var, Some(tid))
            .map_err(|_| todo!())?;
        Ok(InferenceStatus::Solved)
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
    ) -> Result<InferenceStatus, InferenceErrorKind>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        let (left, left_tid) = self.normalize_ty(env, unnorm_left, meta)?;
        let (right, right_tid) = self.normalize_ty(env, unnorm_right, meta)?;

        match (left, right) {
            (a, b) if a == b => Ok(InferenceStatus::Solved),
            (Type::Builtin(left), Type::Builtin(right)) if left == right => {
                Ok(InferenceStatus::Solved)
            }
            (Type::Builtin(BuiltinType::CVarArgs), _)
                if self.type_is_c_vararg_compatible(env, right_tid, meta) =>
            {
                Ok(InferenceStatus::Solved)
            }
            (_, Type::Builtin(BuiltinType::CVarArgs)) => {
                unreachable!("Users shouldn't be able to construct `...`")
            }
            (Type::Builtin(_), Type::Builtin(_)) => Err(InferenceErrorKind::TypesIncompatible {
                expected: left_tid,
                found: right_tid,
            }),
            (Type::RigidTypeVar(left), Type::RigidTypeVar(right)) if left == right => {
                Ok(InferenceStatus::Solved)
            }
            (Type::TypeVar(left), Type::TypeVar(right)) => {
                self.lookup
                    .unify_keys(left, right)
                    .map_err(|UnifiactionError(_, _)| todo!())?;
                self.revision += 1;
                Ok(InferenceStatus::Solved)
            }
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
            (Type::Projection(_), _) | (_, Type::Projection(_)) => Ok(InferenceStatus::Defer),
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
                let status = left
                    .args
                    .iter()
                    .zip(right.args.iter())
                    .map(|(left, right)| self.unify_ty_ty(env, *left, *right, meta))
                    .collect::<Result<Vec<InferenceStatus>, InferenceErrorKind>>()?;

                if status.iter().any(|s| *s == InferenceStatus::Defer) {
                    Ok(InferenceStatus::Defer)
                } else {
                    Ok(InferenceStatus::Solved)
                }
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

                let mut status = InferenceStatus::Solved;

                assert!(left.args.len() == right.args.len());
                for (left, right) in left.args.into_iter().zip(right.args.into_iter()) {
                    match self.unify_ty_ty(env, left, right, meta)? {
                        InferenceStatus::Defer => status = InferenceStatus::Defer,
                        _ => (),
                    };
                }

                Ok(status)
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

                let mut status = InferenceStatus::Solved;

                assert!(left.args.len() == right.args.len());
                for (left, right) in left.args.into_iter().zip(right.args.into_iter()) {
                    match self.unify_ty_ty(env, left, right, meta)? {
                        InferenceStatus::Defer => status = InferenceStatus::Defer,
                        _ => (),
                    };
                }

                Ok(status)
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

                let mut status = InferenceStatus::Solved;

                assert!(left.len() == right.len());
                for (left, right) in left.into_iter().zip(right.into_iter()) {
                    match self.unify_ty_ty(env, left, right, meta)? {
                        InferenceStatus::Defer => status = InferenceStatus::Defer,
                        _ => (),
                    };
                }

                Ok(status)
            }
            (Type::Tuple(_), _) | (_, Type::Tuple(_)) => {
                Err(InferenceErrorKind::TypesIncompatible {
                    expected: left_tid,
                    found: right_tid,
                })
            }
            (Type::Function(left), Type::Function(right)) => {
                if (left.input.len() != right.input.len()) {
                    return Err(InferenceErrorKind::TypesIncompatible {
                        expected: left_tid,
                        found: right_tid,
                    });
                }

                let mut status = InferenceStatus::Defer;

                for (left, right) in left.input.into_iter().zip(right.input.into_iter()) {
                    match self.unify_ty_ty(env, left, right, meta)? {
                        InferenceStatus::Defer => status = InferenceStatus::Defer,
                        _ => (),
                    }
                }

                match (left.output, right.output) {
                    (StackState::Unreachable, StackState::Unreachable) => (),
                    (StackState::Reachable(left), StackState::Reachable(right)) => {
                        if left.len() != right.len() {
                            return Err(InferenceErrorKind::TypesIncompatible {
                                expected: left_tid,
                                found: right_tid,
                            });
                        }

                        for (left, right) in left.into_iter().zip(right) {
                            match self.unify_ty_ty(env, left, right, meta)? {
                                InferenceStatus::Defer => status = InferenceStatus::Defer,
                                _ => (),
                            }
                        }
                    }

                    _ => {
                        return Err(InferenceErrorKind::TypesIncompatible {
                            expected: left_tid,
                            found: right_tid,
                        });
                    }
                }

                Ok(status)
            }
            (_, _) => todo!(
                "{meta:?}: Unify {}, {}",
                self.fmt(left_tid),
                self.fmt(right_tid)
            ),
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
    ) -> Result<InferenceStatus, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        assert!(variances.len() == sub_args.len());
        assert!(variances.len() == super_args.len());

        let mut status = InferenceStatus::Solved;

        for ((variance, sub_tid), super_tid) in variances.iter().zip(sub_args).zip(super_args) {
            let tid = match variance {
                Variance::Covariant => {
                    match self.check_subtype(env, *sub_tid, *super_tid, origin, meta)? {
                        InferenceStatus::Defer => status = InferenceStatus::Defer,
                        _ => (),
                    }
                }
                Variance::Invariant => {
                    match self
                        .unify_ty_ty(env, *super_tid, *sub_tid, meta)
                        .map_err(|kind| InferenceError {
                            kind,
                            origin: origin.clone(),
                            meta: meta.clone(),
                        })? {
                        InferenceStatus::Defer => status = InferenceStatus::Defer,
                        _ => (),
                    }
                }
                Variance::Contravariant => match self
                    .check_subtype(env, *super_tid, *sub_tid, origin, meta)
                    .map_err(|e| match e.kind {
                        InferenceErrorKind::TypesIncompatible { expected, found } => {
                            InferenceError::types_incompatible(found, expected, origin, &e.meta)
                        }
                        _ => e,
                    })? {
                    InferenceStatus::Defer => status = InferenceStatus::Defer,
                    _ => (),
                },
            };
        }

        Ok(status)
    }

    pub fn check_subtype(
        &mut self,
        env: &Environment,
        subty: TypeId,
        superty: TypeId,
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Result<InferenceStatus, InferenceError<M>>
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
                let mut status = InferenceStatus::Solved;
                for (sub, sup) in sub.into_iter().zip(sup.into_iter()) {
                    match self.check_subtype(env, sub, sup, origin, meta)? {
                        InferenceStatus::Defer => status = InferenceStatus::Defer,
                        _ => (),
                    }
                }

                Ok(status)
            }
            (Type::Function(sub_f), Type::Function(super_f)) => {
                if sub_f.input.len() != super_f.input.len() {
                    todo!("Incompatible input arity");
                }

                let mut status = InferenceStatus::Solved;

                for (subty, superty) in sub_f.input.iter().zip(super_f.input.iter()) {
                    match self.check_subtype(env, *superty, *subty, origin, meta)? {
                        InferenceStatus::Defer => status = InferenceStatus::Defer,
                        _ => (),
                    }
                }

                match (sub_f.output, super_f.output) {
                    (StackState::Unreachable, _) => (),
                    (StackState::Reachable(type_ids), StackState::Unreachable) => todo!(),

                    (StackState::Reachable(sub), StackState::Reachable(sup)) => {
                        if (sub.len() != sup.len()) {
                            todo!("Incompatible output arity")
                        }

                        for (subty, superty) in sub.iter().zip(sup.iter()) {
                            match self.check_subtype(env, *subty, *superty, origin, meta)? {
                                InferenceStatus::Defer => status = InferenceStatus::Defer,
                                _ => (),
                            }
                        }
                    }
                }

                Ok(status)
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
    ) -> Result<ImplResolution, InferenceErrorKind> {
        let (unbound, args) = self.substitute_stack(&args);
        if !unbound.is_empty() {
            return Ok(ImplResolution::Defer);
        }

        let mut matching_impls = vec![];
        let mut defer = false;

        match env.get_impls(&cid) {
            Some(impls) => {
                'candidate: for i in impls {
                    let i = self.freshen_impl(i, meta);
                    assert_eq!(i.args.len(), args.len());

                    let mut candidate_defer = false;

                    for (expected, found) in i.args.iter().zip(args.iter()) {
                        match self.unify_ty_ty(env, *expected, *found, meta) {
                            Ok(InferenceStatus::Defer) => candidate_defer = true,
                            Ok(InferenceStatus::Solved) => (),
                            Err(_) => continue 'candidate,
                        }
                    }

                    for p in &i.predicates {
                        match self.get_matching_impl(env, p.cid, &p.args, meta)? {
                            ImplResolution::Match(_) => (),
                            ImplResolution::Defer => candidate_defer = true,
                            ImplResolution::Unmatched => continue 'candidate,
                        }
                    }

                    if candidate_defer {
                        defer = true;
                    } else {
                        matching_impls.push(i);
                    }
                }

                match matching_impls.len() {
                    _ if defer => Ok(ImplResolution::Defer),
                    0 => Ok(ImplResolution::Unmatched),
                    1 => Ok(ImplResolution::Match(matching_impls.pop().unwrap())),
                    _ => {
                        if matching_impls
                            .iter()
                            .all(|i| i.coherence == Coherence::Overlapping)
                        {
                            Ok(ImplResolution::Match(matching_impls.pop().unwrap()))
                        } else {
                            todo!("Multiple matching implementations with unique coherence");
                        }
                    }
                }
            }
            None => Err(InferenceErrorKind::PredicateNotMet {
                cid,
                args: args.clone(),
            }),
        }
    }

    fn check_predicate(
        &mut self,
        env: &Environment,
        predicate: &TypePredicate,
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Result<InferenceStatus, InferenceError<M>>
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
            ImplResolution::Match(_) => Ok(InferenceStatus::Solved),
            ImplResolution::Defer => Ok(InferenceStatus::Defer),
            ImplResolution::Unmatched => {
                let (_, args) = self.substitute_stack(&predicate.args);
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
    ) -> Result<InferenceStatus, InferenceError<M>>
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

    fn check_constraint<Lookup>(
        &mut self,
        env: &Environment,
        constraint: &Constraint<M>,
        lookup: &Lookup,
    ) -> Result<InferenceStatus, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
        Lookup: MemberLookup,
    {
        let r = match &constraint.constraint {
            ConstraintType::Equal(left, right) => self
                .unify_ty_ty(env, *left, *right, &constraint.meta)
                .map_err(|kind| InferenceError {
                    kind,
                    origin: constraint.origin.clone(),
                    meta: constraint.meta.clone(),
                }),
            ConstraintType::Subtype { subty, superty } => {
                self.check_subtype(env, *subty, *superty, &constraint.origin, &constraint.meta)
            }
            ConstraintType::Join { output, inputs } => {
                self.check_join(env, *output, inputs, &constraint.origin, &constraint.meta)
            }
            ConstraintType::Predicate(pred) => {
                self.check_predicate(env, pred, &constraint.origin, &constraint.meta)
            }
            ConstraintType::Unreachable(stack) => {
                if stack.is_reachable() {
                    Err(InferenceError::unreachable_state_reached(
                        stack.clone(),
                        &constraint.origin,
                        &constraint.meta,
                    ))
                } else {
                    Ok(InferenceStatus::Solved)
                }
            }
            ConstraintType::Arity { expected, found } => {
                if expected.len() != found.len() {
                    return Err(InferenceError::stacks_differ(
                        expected.clone(),
                        found.clone(),
                        &constraint.origin,
                        &constraint.meta,
                    ));
                } else {
                    Ok(InferenceStatus::Solved)
                }
            }
            ConstraintType::HasEnoughArgs { expected, found } => {
                if expected.len() > found.len() {
                    return Err(InferenceError::not_enough_args(
                        expected.clone(),
                        found.clone(),
                        &constraint.origin,
                        &constraint.meta,
                    ));
                } else {
                    Ok(InferenceStatus::Solved)
                }
            }
            ConstraintType::HasMember {
                base_tid,
                member,
                member_tid,
            } => {
                let base_tid = self
                    .normalize_ty(env, *base_tid, &constraint.meta)
                    .unwrap()
                    .1;
                let member_tid = self
                    .normalize_ty(env, *member_tid, &constraint.meta)
                    .unwrap()
                    .1;

                match member {
                    Member::Named(member) => {
                        match self.get_member_idx(base_tid, member, lookup, &constraint.meta)? {
                            Some(info) => {
                                let c = Constraint::new(
                                    ConstraintType::Equal(info.tid, member_tid),
                                    ConstraintOrigin::PlaceProjection,
                                    &constraint.meta,
                                );
                                self.check_constraint(env, &c, lookup)
                            }
                            None => Ok(InferenceStatus::Defer),
                        }
                    }
                    Member::Idx(idx) => {
                        match self.get_member_from_idx(base_tid, *idx, lookup, &constraint.meta)? {
                            Some(info) => {
                                let c = Constraint::new(
                                    ConstraintType::Equal(info.tid, member_tid),
                                    ConstraintOrigin::PlaceProjection,
                                    &constraint.meta,
                                );
                                self.check_constraint(env, &c, lookup)
                            }
                            None => Ok(InferenceStatus::Defer),
                        }
                    }
                }
            }
        }?;
        Ok(r)
    }

    fn check_constraints<Lookup>(
        &mut self,
        env: &Environment,
        constraints: &mut Vec<Constraint<M>>,
        lookup: &Lookup,
    ) -> Result<(), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
        Lookup: MemberLookup,
    {
        // constraints.sort();

        let mut pending: Vec<_> = constraints.iter().collect();

        while !pending.is_empty() {
            let revision = self.revision;
            let previous_len = pending.len();
            let mut deferred = Vec::new();

            for c in pending {
                match self.check_constraint(env, c, lookup)? {
                    InferenceStatus::Solved => {}
                    InferenceStatus::Defer => {
                        deferred.push(c);
                    }
                }
            }

            if deferred.is_empty() {
                return Ok(());
            }

            let changed = self.revision != revision || deferred.len() != previous_len;

            if !changed && !deferred.is_empty() {
                todo!()
            }

            pending = deferred;
        }

        Ok(())
    }

    pub fn type_check<'src, Lookup>(
        &mut self,
        expr: Expr<'src, M>,
        stack: &Stack,
        target: &StackState,
        env: &mut Environment,
        lookup: &Lookup,
    ) -> Result<(Expr<'src, (M, TypeId)>, HashSet<TypeVar>), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
        Lookup: MemberLookup,
    {
        // Determine which types are unbound at the start.

        let (mut unbound, stack) = self.substitute_stack(stack);
        let mut stack = StackState::Reachable(stack);
        // Get constraints from inference
        let (out_expr, mut constraints) = self.infer(expr, &mut stack, env, lookup)?;

        // Add constraint that the target and stack must be equal.
        match target {
            StackState::Reachable(target) => {
                constraints.extend(Constraint::output_subtyping(
                    target,
                    &stack,
                    &out_expr.meta.0,
                ));
            }
            StackState::Unreachable => constraints.push(Constraint::new(
                ConstraintType::Unreachable(stack.clone()),
                ConstraintOrigin::OutputUnreachable,
                &out_expr.meta.0,
            )),
        }

        // Make sure that constraints can be resolved.
        self.check_constraints(&env, &mut constraints, lookup)?;

        let (unbound_stack, _) = self.substitute_stack_state(&stack);
        let (unbound_expr, expr) = self.substitute_expr(out_expr, lookup);

        unbound.extend(unbound_stack);
        unbound.extend(unbound_expr);

        Ok((expr, unbound))
    }

    pub fn type_instance(
        &mut self,
        env: &Environment,
        tid: TypeId,
        args: Option<Vec<TypeId>>,
        meta: &M,
    ) -> Result<TypeId, InferenceErrorKind>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        match self.get(tid).expect("TypeIds should be resolvable") {
            Type::Struct(s) => {
                let s = s.clone();
                let args = if let Some(args) = args {
                    if s.args.len() != args.len() {
                        return Err(InferenceErrorKind::WrongNumberOfArgs {
                            expected: s.args,
                            found: args,
                        });
                    }

                    args
                } else {
                    let s = s.clone();
                    let vars = self.get_struct_def(s.sid).vars.clone();
                    let mut fresh = self.freshen_vars(&vars, meta);
                    self.freshen_many_tids(&s.args, &mut fresh, meta)
                };
                Ok(self.intern(Type::Struct(StructType { sid: s.sid, args })))
            }
            Type::TypeVar(_)
            | Type::RigidTypeVar(_)
            | Type::Builtin(_)
            | Type::Function(_)
            | Type::Pointer(_)
            | Type::Tuple(_)
            | Type::Type(_) => {
                if let Some(args) = args {
                    return Err(InferenceErrorKind::WrongNumberOfArgs {
                        expected: vec![],
                        found: args,
                    });
                }
                Ok(tid)
            }
            Type::Enum(e) => {
                let e = e.clone();
                let args = if let Some(args) = args {
                    if e.args.len() != args.len() {
                        return Err(InferenceErrorKind::WrongNumberOfArgs {
                            expected: e.args,
                            found: args,
                        });
                    }

                    args
                } else {
                    let e = e.clone();
                    let vars = self.get_enum_def(e.eid).vars.clone();
                    let mut fresh = self.freshen_vars(&vars, meta);
                    self.freshen_many_tids(&e.args, &mut fresh, meta)
                };
                Ok(self.intern(Type::Enum(EnumType { eid: e.eid, args })))
            }
            Type::Variant(variant) => {
                let v = variant.clone();
                let args = if let Some(args) = args {
                    if v.args.len() != args.len() {
                        return Err(InferenceErrorKind::WrongNumberOfArgs {
                            expected: v.args,
                            found: args,
                        });
                    }

                    args
                } else {
                    let v = v.clone();
                    let vars = self.get_enum_def(v.eid).vars.clone();
                    let mut fresh = self.freshen_vars(&vars, meta);
                    self.freshen_many_tids(&v.args, &mut fresh, meta)
                };

                Ok(self.intern(Type::Variant(VariantType {
                    eid: v.eid,
                    args,
                    variant: v.variant,
                })))
            }
            Type::Projection(p) => {
                let p = p.clone();
                let args = if let Some(args) = args {
                    if p.args.len() != args.len() {
                        return Err(InferenceErrorKind::WrongNumberOfArgs {
                            expected: p.args,
                            found: args,
                        });
                    }

                    args
                } else {
                    let vars = env.get_typeclass(p.cid).vars.clone();
                    let mut fresh = self.freshen_vars(&vars, meta);
                    self.freshen_many_tids(&p.args, &mut fresh, meta)
                };
                Ok(self.intern(Type::Projection(Projection {
                    cid: p.cid,
                    args,
                    idx: p.idx,
                })))
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
                func.output = match func.output {
                    StackState::Reachable(output) => StackState::Reachable(
                        output
                            .into_iter()
                            .map(|tid| self.replace(tid, replacements))
                            .collect(),
                    ),
                    StackState::Unreachable => StackState::Unreachable,
                };
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
            | Type::Projection(_) => todo!("{} is not an enum", self.fmt(enum_tid)),
        };

        Ok(self.intern(Type::Variant(variant)))
    }

    pub fn metatype(
        &mut self,
        env: &Environment,
        tid: TypeId,
        args: Option<Vec<TypeId>>,
        meta: &M,
    ) -> Result<TypeId, InferenceErrorKind>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        let tid = self.type_instance(env, tid, args, meta)?;
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
                .chain(match &func.output {
                    StackState::Reachable(tids) => tids.iter(),
                    StackState::Unreachable => [].iter(),
                })
                .any(|tid| self.is_free(tid)),
            Type::Pointer(p) => self.is_free(&p.tid),
            Type::Struct(s) => s.args.iter().any(|tid| self.is_free(tid)),
            Type::Enum(e) => e.args.iter().any(|tid| self.is_free(tid)),
            Type::Tuple(tids) => tids.iter().any(|tid| self.is_free(tid)),
            Type::Variant(v) => v.args.iter().any(|tid| self.is_free(tid)),
            Type::Type(tid) => self.is_free(tid),
            Type::Projection(p) => p.args.iter().any(|tid| self.is_free(tid)),
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
        let lower_tid = self.lower(scheme.tid);
        let scheme_tid = self.freshen_tid(lower_tid, &mut fresh, meta);

        let tid = self.freshen_tid(tid, &mut HashMap::new(), meta);

        self.unify_ty_ty(env, scheme_tid, tid, meta)?;

        let mut tids = vec![];
        for tid in vars {
            tids.push(self.normalize_ty(env, tid, meta)?.1);
        }

        Ok(tids)
    }

    pub fn lower(&mut self, tid: TypeId) -> TypeId {
        match self.get(tid).unwrap().clone() {
            Type::Builtin(_) | Type::TypeVar(_) => tid,
            Type::RigidTypeVar(tyvar) => self.intern(Type::TypeVar(tyvar)),
            Type::Function(FunctionType { input, output }) => {
                let input = input.into_iter().map(|tid| self.lower(tid)).collect();
                let output = match output {
                    StackState::Reachable(output) => StackState::Reachable(
                        output.into_iter().map(|tid| self.lower(tid)).collect(),
                    ),
                    StackState::Unreachable => StackState::Unreachable,
                };
                self.function(FunctionType { input, output })
            }
            Type::Pointer(p) => {
                let tid = self.lower(p.tid);
                self.pointer(tid, p.mutability)
            }
            Type::Struct(s) => {
                let args = s.args.into_iter().map(|tid| self.lower(tid)).collect();
                self.intern(Type::Struct(StructType { sid: s.sid, args }))
            }
            Type::Enum(e) => {
                let args = e.args.into_iter().map(|tid| self.lower(tid)).collect();
                self.intern(Type::Enum(EnumType { eid: e.eid, args }))
            }
            Type::Tuple(tids) => {
                let tids = tids.into_iter().map(|tid| self.lower(tid)).collect();
                self.intern(Type::Tuple(tids))
            }
            Type::Variant(v) => {
                let args = v.args.into_iter().map(|tid| self.lower(tid)).collect();
                self.intern(Type::Variant(VariantType {
                    eid: v.eid,
                    args,
                    variant: v.variant,
                }))
            }
            Type::Type(tid) => {
                let tid = self.lower(tid);
                self.intern(Type::Type(tid))
            }
            Type::Projection(p) => {
                let args = p.args.into_iter().map(|tid| self.lower(tid)).collect();
                self.intern(Type::Projection(Projection {
                    cid: p.cid,
                    args,
                    idx: p.idx,
                }))
            }
        }
    }
}
