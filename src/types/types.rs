use crate::ast::arg::Arg;
use crate::ast::expr::UntypedExpr;
use crate::ast::stmt::Member;
use crate::error::HayError;
use crate::lex::token::{Loc, Token, TokenKind, TypeToken};
use crate::types::Typed;
use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeId(pub String);

impl TypeId {
    pub fn new<S: Into<String>>(id: S) -> Self {
        TypeId(id.into())
    }

    pub fn from_token(
        token: &Token,
        types: &mut BTreeMap<TypeId, Type>,
        local_types: &Vec<TypeId>,
    ) -> Result<TypeId, HayError> {
        if types.contains_key(&TypeId::new(&token.lexeme))
            || local_types.iter().find(|t| t.0 == token.lexeme).is_some()
        {
            return Ok(TypeId(token.lexeme.clone()));
        }

        let typ = match &token.kind {
            TokenKind::Type(typ) => typ,
            _ => panic!("Didn't expect this...: {:?}", token.kind),
        };

        TypeId::from_type_token(token, typ, types, local_types)
    }

    fn from_type_token(
        token: &Token,
        typ: &TypeToken,
        types: &mut BTreeMap<TypeId, Type>,
        local_types: &Vec<TypeId>,
    ) -> Result<TypeId, HayError> {
        match typ {
            TypeToken::Array { base, .. } => {
                let mut map = HashMap::new();
                let base_tid = TypeId::from_type_token(token, base, types, local_types)?;

                map.insert(TypeId::new("T"), base_tid);
                let arr_tid = TypeId::new("Arr").assign(token, &map, types)?;
                // TODO: figure out why this doesn't need to be a pointer type...
                Ok(arr_tid)
            }
            TypeToken::Base(base) => {
                if types.contains_key(&TypeId::new(base))
                    || local_types.iter().find(|t| &t.0 == base).is_some()
                {
                    Ok(TypeId(base.clone()))
                } else {
                    Err(HayError::new(
                        format!("Unrecognized type: {base}"),
                        token.loc.clone(),
                    ))
                }
            }
            TypeToken::Parameterized { base, inner } => {
                let base_tid = TypeId::new(base);
                match types.get(&base_tid).cloned() {
                    Some(Type::GenericRecordBase {
                        generics,
                        members,
                        kind,
                        ..
                    }) => {
                        if generics.len() != inner.len() {
                            return Err(HayError::new(format!("Incorrect number of type annotations provided. Expected annotations for {:?}", generics), token.loc.clone()));
                        }

                        let mut annotations = vec![];
                        for t in inner {
                            annotations.push(TypeId::from_type_token(
                                &token,
                                t,
                                types,
                                local_types,
                            )?);
                        }

                        if annotations.iter().any(|t| types.get(t).is_none()) {
                            let t = Type::GenericRecordInstance {
                                base: TypeId::new(base),
                                base_generics: generics.clone(),
                                alias_list: annotations,
                                members: members.clone(),
                                kind: kind.clone(),
                            };
                            let tid = t.id();

                            types.insert(tid.clone(), t);
                            Ok(tid)
                        } else {
                            let map: HashMap<TypeId, TypeId> =
                                generics.into_iter().zip(annotations.into_iter()).collect();
                            base_tid.assign(token, &map, types)
                        }
                    }
                    Some(_) => {
                        return Err(HayError::new(
                            format!("Type {base} cannot be annotated, because it is not generic."),
                            token.loc.clone(),
                        ));
                    }
                    None => {
                        return Err(HayError::new(
                            format!("Unrecognized base type: {base}"),
                            token.loc.clone(),
                        ));
                    }
                }
            }
            TypeToken::Pointer(inner) => {
                let inner_typ_id = TypeId::from_type_token(token, inner, types, local_types)?;
                let t = Type::Pointer {
                    inner: inner_typ_id,
                };

                let tid = t.id();

                types.insert(tid.clone(), t);

                Ok(tid)
            }
        }
    }

    pub fn assign(
        &self,
        token: &Token,
        map: &HashMap<TypeId, TypeId>,
        types: &mut BTreeMap<TypeId, Type>,
    ) -> Result<TypeId, HayError> {
        if let Some(new_t) = map.get(&self) {
            return Ok(new_t.clone());
        }

        let maybe_typ = types.get(&self).cloned();
        match maybe_typ {
            Some(typ) => match typ {
                Type::GenericRecordBase {
                    token: base_token,
                    name: name_token,
                    generics,
                    members,
                    kind,
                    ..
                } => {
                    let mut resolved_members = vec![];
                    for m in members {
                        resolved_members.push(Member {
                            vis: m.vis,
                            token: m.token,
                            ident: m.ident,
                            typ: Typed(m.typ.0.assign(token, map, types)?),
                        });
                    }
                    let mut resolved_generics = vec![];
                    for t in generics {
                        resolved_generics.push(t.assign(token, map, types)?);
                    }

                    let mut name = format!("{self}<");
                    for t in &resolved_generics[0..resolved_generics.len() - 1] {
                        name = format!("{name}{t} ");
                    }

                    let name = TypeId::new(format!("{name}{}>", resolved_generics.last().unwrap()));

                    let t = Type::Record {
                        token: base_token,
                        name: Token {
                            kind: name_token.kind,
                            lexeme: name.0.clone(),
                            loc: name_token.loc,
                        },
                        members: resolved_members,
                        kind: kind,
                    };

                    types.insert(name.clone(), t);
                    Ok(name)
                }
                Type::GenericRecordInstance {
                    base,
                    base_generics,
                    alias_list,
                    members,
                    kind,
                } => {
                    let alias_map: HashMap<TypeId, TypeId> = HashMap::from_iter(
                        base_generics
                            .clone()
                            .into_iter()
                            .zip(alias_list.into_iter()),
                    );

                    let mut aliased_generics = HashMap::new();
                    for (k, v) in &alias_map {
                        aliased_generics.insert(k.clone(), v.clone().assign(token, map, types)?);
                    }

                    let mut resolved_members = vec![];
                    for member in members {
                        resolved_members.push(Member {
                            vis: member.vis,
                            token: member.token,
                            ident: member.ident,
                            typ: Typed(member.typ.0.assign(token, &aliased_generics, types)?),
                        });
                    }

                    let mut resolved_generics = vec![];
                    for gen in base_generics {
                        resolved_generics.push(gen.assign(token, &aliased_generics, types)?)
                    }

                    let mut name = format!("{base}<");
                    for t in &resolved_generics[0..resolved_generics.len() - 1] {
                        name = format!("{name}{t} ");
                    }

                    name = format!("{name}{}>", resolved_generics.last().unwrap());

                    let t = Type::Record {
                        token: token.clone(),
                        name: Token::new(
                            TokenKind::Type(TypeToken::Base(name.clone())),
                            name.clone(),
                            token.loc.file.clone(),
                            token.loc.line.clone(),
                            token.loc.span.start,
                            token.loc.span.end,
                        ),
                        members: resolved_members,
                        kind,
                    };

                    let tid = t.id();

                    types.insert(tid.clone(), t);

                    Ok(tid)
                }
                Type::Pointer { inner } => {
                    let inner = inner.assign(token, map, types)?;
                    let t = Type::Pointer { inner };
                    let id: TypeId = t.id();

                    types.insert(id.clone(), t);
                    Ok(id)
                }
                Type::Char
                | Type::U64
                | Type::U8
                | Type::Bool
                | Type::Enum { .. }
                | Type::Record { .. } => Ok(self.clone()),
                Type::GenericFunction {
                    token: fn_token,
                    name,
                    inputs,
                    outputs,
                    generics,
                    body,
                } => {
                    if !generics.iter().all(|tid| map.contains_key(tid))
                        && map.len() == generics.len()
                    {
                        return Err(HayError::new(
                            "Bad mapping to monomporphize funcion",
                            token.loc.clone(),
                        )
                        .with_hint(format!(
                            "Generic function {} is generic over {:?}.",
                            name.lexeme, generics
                        ))
                        .with_hint(format!("Found mapping: {:?}", map)));
                    }

                    let mut assigned_inputs = vec![];
                    for input in inputs {
                        assigned_inputs.push(Arg {
                            token: input.token,
                            ident: input.ident,
                            typ: Typed(input.typ.0.assign(&token, map, types)?),
                        });
                    }

                    let mut assigned_outputs = vec![];
                    for output in outputs {
                        assigned_outputs.push(Arg {
                            token: output.token,
                            ident: output.ident,
                            typ: Typed(output.typ.0.assign(&token, map, types)?),
                        });
                    }

                    let mut name_string = format!("{}<", name.lexeme);
                    for tid in &generics[0..generics.len() - 1] {
                        name_string = format!("{name_string}{tid} ");
                    }
                    name_string = format!("{name_string}{}>", generics.last().unwrap());

                    let tid = TypeId::new(&name_string);

                    let new_fn = Type::UncheckedFunction {
                        token: fn_token,
                        name: Token {
                            kind: name.kind,
                            lexeme: name_string,
                            loc: name.loc,
                        },
                        inputs: assigned_inputs,
                        outputs: assigned_outputs,
                        body: body,
                        generic_map: Some(map.clone()),
                    };

                    types.insert(tid.clone(), new_fn);

                    Ok(tid)
                }
                Type::UncheckedFunction { .. } => {
                    unreachable!("Should never assign to non-generic function!")
                }
            },
            None => {
                assert!(
                    map.contains_key(&self),
                    "Expected to find {self} in {:?}",
                    map
                );

                Ok(map.get(&self).unwrap().clone())
            }
        }
    }

    pub fn resolve(
        &self,
        token: &Token,
        concrete: &Self,
        map: &mut HashMap<Self, Self>,
        types: &mut BTreeMap<Self, Type>,
    ) -> Result<Self, HayError> {
        match (types.get(self).cloned(), types.get(concrete).cloned()) {
            (None, None) => {
                if self != concrete {
                    return Err(HayError::new(
                        format!("Cannot resolve generic type {self} from {concrete}"),
                        token.loc.clone(),
                    ));
                }

                if !map.contains_key(&concrete) {
                    return Err(HayError::new(
                        format!("Generic type {self} has not been mapped to a concrete type."),
                        token.loc.clone(),
                    )
                    .with_hint("The following types have been mapped:")
                    .with_hint(format!("{:?}", map)));
                }

                Ok(map.get(self).unwrap().clone())
            }
            (None, Some(_)) => {
                if let Some(prev) = map.insert(self.clone(), concrete.clone()) {
                    if &prev != concrete {
                        return Err(HayError::new_type_err(
                            "Conflict in type resolution",
                            token.loc.clone(),
                        )
                        .with_hint(format!("Failed to resolve generic type {self}"))
                        .with_hint(format!("Tried to resolve to both {prev} and {concrete}")));
                    }
                }

                Ok(concrete.clone())
            }
            (Some(Type::U64), Some(Type::U64))
            | (Some(Type::U8), Some(Type::U8))
            | (Some(Type::Char), Some(Type::Char))
            | (Some(Type::Bool), Some(Type::Bool)) => Ok(concrete.clone()),
            (
                Some(Type::Pointer { inner }),
                Some(Type::Pointer {
                    inner: inner_concrete,
                }),
            ) => {
                let p = Type::Pointer {
                    inner: inner.resolve(token, &inner_concrete, map, types)?,
                };
                let tid = p.id();
                types.insert(tid.clone(), p);
                Ok(tid)
            }
            (Some(Type::Record { kind, .. }), Some(Type::Record { .. })) => {
                if self != concrete {
                    return Err(HayError::new_type_err(
                        format!("Cannot resolve {kind} {self} from {concrete}."),
                        token.loc.clone(),
                    ));
                }

                Ok(self.clone())
            }
            (Some(Type::Record { .. }), Some(_)) => Err(HayError::new_type_err(
                format!("Cannot resolve {self} from {concrete}"),
                token.loc.clone(),
            )),
            (
                Some(Type::GenericRecordInstance {
                    members: generic_members,
                    base,
                    kind,
                    alias_list,
                    base_generics,
                }),
                Some(Type::Record { members, name, .. }),
            ) => {
                if !name.lexeme.starts_with(&base.0) {
                    return Err(HayError::new(
                        format!("Cannot resolve {kind} {base} from {}", name.lexeme),
                        token.loc.clone(),
                    ));
                }

                assert!(members.len() == generic_members.len());

                for (generic, concrete) in generic_members.iter().zip(members) {
                    generic.typ.0.resolve(token, &concrete.typ.0, map, types)?;
                }

                for (old, new) in base_generics.iter().zip(alias_list) {
                    if let Some(tid) = map.remove(old) {
                        map.insert(new, tid);
                    } else {
                        return Err(HayError::new_type_err(
                            format!("Generic type {new}, was not defined"),
                            token.loc.clone(),
                        ));
                    }
                }

                Ok(concrete.clone())
            }
            (
                Some(Type::Enum { name, .. }),
                Some(Type::Enum {
                    name: concrete_name,
                    ..
                }),
            ) => {
                if name.lexeme != concrete_name.lexeme {
                    return Err(HayError::new(
                        format!(
                            "Failed to resolve enum type {} from {}",
                            name.lexeme, concrete_name.lexeme
                        ),
                        token.loc.clone(),
                    ));
                }

                Ok(concrete.clone())
            }

            (Some(Type::Pointer { .. }), _)
            | (Some(Type::Bool), _)
            | (Some(Type::Char), _)
            | (Some(Type::U8), _)
            | (Some(Type::U64), _)
            | (Some(Type::Enum { .. }), _)
            | (Some(Type::GenericRecordInstance { .. }), _)
            | (Some(Type::Record { .. }), _) => Err(HayError::new_type_err(
                format!("Cannot resolve {self} from {concrete}"),
                token.loc.clone(),
            )),
            (Some(Type::GenericRecordBase { .. }), _) => unreachable!(
                "Generic bases shouldn't be part of type resolution, only Generic Instances.",
            ),
            (Some(Type::UncheckedFunction { .. } | Type::GenericFunction { .. }), _) => {
                unreachable!("Functions should never be part of type resolution.")
            }
        }
    }

    pub fn size(&self, types: &BTreeMap<TypeId, Type>) -> Result<usize, HayError> {
        match types.get(&self).unwrap() {
            Type::Bool
            | Type::Char
            | Type::U64
            | Type::U8
            | Type::Enum { .. }
            | Type::Pointer { .. } => Ok(1),
            Type::Record { members, kind, .. } => match kind {
                RecordKind::Struct => {
                    let mut sum = 0;
                    for member in members {
                        sum += member.typ.0.size(types)?;
                    }
                    Ok(sum)
                }
                RecordKind::Union => {
                    let mut max = 0;
                    for member in members {
                        let sz = member.typ.0.size(types)?;
                        if sz > max {
                            max = sz;
                        }
                    }

                    Ok(max)
                }
            },
            Type::GenericRecordBase { .. } | Type::GenericRecordInstance { .. } => {
                Err(HayError::new(
                    "Generic Records do not have a size known at compile time",
                    Loc::new("", 0, 0, 0),
                ))
            }
            Type::UncheckedFunction { .. } | Type::GenericFunction { .. } => Err(HayError::new(
                "Functions do not have a size",
                Loc::new("", 0, 0, 0),
            )),
        }
    }
}

impl std::fmt::Debug for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RecordKind {
    Struct,
    Union,
}

impl std::fmt::Display for RecordKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RecordKind::Struct => write!(f, "struct"),
            RecordKind::Union => write!(f, "union"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    U8,
    U64,
    Char,
    Bool,
    Pointer {
        inner: TypeId,
    },
    Record {
        token: Token,
        name: Token,
        members: Vec<Member<Typed>>,
        kind: RecordKind,
    },
    GenericRecordBase {
        token: Token,
        name: Token,
        generics: Vec<TypeId>,
        members: Vec<Member<Typed>>,
        kind: RecordKind,
    },
    GenericRecordInstance {
        base: TypeId, //
        base_generics: Vec<TypeId>,
        alias_list: Vec<TypeId>,
        members: Vec<Member<Typed>>,
        kind: RecordKind,
    },
    Enum {
        token: Token,
        name: Token,
        variants: Vec<Token>,
    },
    GenericFunction {
        token: Token,
        name: Token,
        inputs: Vec<Arg<Typed>>,
        outputs: Vec<Arg<Typed>>,
        generics: Vec<TypeId>,
        body: Vec<Box<UntypedExpr>>,
    },
    UncheckedFunction {
        token: Token,
        name: Token,
        inputs: Vec<Arg<Typed>>,
        outputs: Vec<Arg<Typed>>,
        body: Vec<Box<UntypedExpr>>,
        generic_map: Option<HashMap<TypeId, TypeId>>,
    },
}

impl Type {
    pub fn id(&self) -> TypeId {
        match self {
            Type::U64 => TypeId::new("u64"),
            Type::U8 => TypeId::new("u8"),
            Type::Char => TypeId::new("char"),
            Type::Bool => TypeId::new("bool"),
            Type::Enum { name, .. }
            | Type::GenericRecordBase { name, .. }
            | Type::Record { name, .. } => TypeId::new(&name.lexeme),
            Type::Pointer { inner } => TypeId::new(format!("*{}", inner.0)),
            Type::GenericRecordInstance {
                base, alias_list, ..
            } => {
                let mut name = format!("{base}<");
                for t in &alias_list[0..alias_list.len() - 1] {
                    name = format!("{name}{t} ");
                }
                name = format!("{name}{}>", alias_list.last().unwrap());

                TypeId::new(name)
            }
            Type::UncheckedFunction { .. } | Type::GenericFunction { .. } => {
                unimplemented!("Haven't implemented name from Functions.")
            }
        }
    }
}

#[derive(Clone)]
pub struct Signature {
    pub inputs: Vec<TypeId>,
    pub outputs: Vec<TypeId>,
    pub generics: Option<Vec<TypeId>>,
}

impl Signature {
    pub fn new(inputs: Vec<TypeId>, outputs: Vec<TypeId>) -> Self {
        Self {
            inputs,
            outputs,
            generics: None,
        }
    }

    pub fn new_maybe_generic(
        inputs: Vec<TypeId>,
        outputs: Vec<TypeId>,
        generics: Option<Vec<TypeId>>,
    ) -> Self {
        Self {
            inputs,
            outputs,
            generics: generics,
        }
    }

    pub fn evaluate(
        &self,
        token: &Token,
        stack: &mut Vec<TypeId>,
        types: &mut BTreeMap<TypeId, Type>,
    ) -> Result<Option<HashMap<TypeId, TypeId>>, HayError> {
        if stack.len() < self.inputs.len() {
            return Err(HayError::new_type_err(
                format!("Invalid number of inputs for {:?}", token.lexeme),
                token.loc.clone(),
            )
            .with_hint(format!("Expected: {:?}", self.inputs))
            .with_hint(format!("Found:    {:?}", stack)));
        }

        let mut map = None;
        let mut to_resolve;
        let sig = if self.generics.is_some() {
            to_resolve = self.clone();
            map = to_resolve.resolve(token, stack, types)?;
            &to_resolve
        } else {
            &self
        };

        for (input, stk) in sig.inputs.iter().rev().zip(stack.iter().rev()) {
            if input != stk {
                return Err(HayError::new_type_err(
                    format!("Type Error - Invalid inputs for {:?}", token.lexeme).as_str(),
                    token.loc.clone(),
                )
                .with_hint(format!("Expected: {:?}", sig.inputs))
                .with_hint(format!(
                    "Found:    {:?}",
                    stack
                        .iter()
                        .rev()
                        .take(sig.inputs.len())
                        .rev()
                        .collect::<Vec<&TypeId>>()
                )));
            }
        }
        for _ in &sig.inputs {
            stack.pop();
        }

        for out in &sig.outputs {
            stack.push(out.clone());
        }

        Ok(map)
    }

    pub fn evaluate_many(
        sigs: &[Signature],
        token: &Token,
        stack: &mut Vec<TypeId>,
        types: &mut BTreeMap<TypeId, Type>,
    ) -> Result<Option<HashMap<TypeId, TypeId>>, HayError> {
        let in_len = sigs[0].inputs.len();
        let out_len = sigs[0].outputs.len();
        if !sigs
            .iter()
            .all(|sig| sig.inputs.len() == in_len && sig.outputs.len() == out_len)
        {
            let mut e = HayError::new(
                "Logic Error - All signatures should have the same input and output lengths for evaluate many.",
                token.loc.clone(),
            ).with_hint("Found these signatures:");

            for sig in sigs {
                e = e.with_hint(format!("{:?}", sig));
            }
            return Err(e);
        }

        for sig in sigs {
            if let Ok(x) = sig.evaluate(token, stack, types) {
                return Ok(x);
            }
        }

        let mut e = HayError::new_type_err(
            format!("Invalid inputs for {}", token.kind),
            token.loc.clone(),
        )
        .with_hint(format!("Expected one of {} signatures:", sigs.len()));

        for sig in sigs {
            e = e.with_hint(format!("  {:?}", sig.inputs));
        }

        e = e.with_hint("Found:");
        e = e.with_hint(format!(
            "  {:?}",
            stack
                .iter()
                .rev()
                .take(in_len)
                .rev()
                .collect::<Vec<&TypeId>>(),
        ));

        Err(e)
    }

    fn resolve(
        &mut self,
        token: &Token,
        stack: &mut Vec<TypeId>,
        types: &mut BTreeMap<TypeId, Type>,
    ) -> Result<Option<HashMap<TypeId, TypeId>>, HayError> {
        let mut map = HashMap::new();
        let len = self.inputs.len();
        for (t, concrete) in self
            .inputs
            .iter_mut()
            .zip(stack.iter().rev().take(len).rev())
        {
            *t = t.resolve(token, concrete, &mut map, types)?;
        }
        for t in &mut self.outputs {
            *t = t.assign(token, &map, types)?;
        }

        if map.len() > 0 {
            Ok(Some(map))
        } else {
            Ok(None)
        }
    }

    pub fn assign(
        &mut self,
        token: &Token,
        annotations: &Vec<TypeId>,
        types: &mut BTreeMap<TypeId, Type>,
    ) -> Result<(), HayError> {
        if self.generics.is_none() {
            return Err(HayError::new_type_err(
                "Cannot assign to non-generic signature.",
                token.loc.clone(),
            ));
        }

        let map: HashMap<TypeId, TypeId> = HashMap::from_iter(
            self.generics
                .as_ref()
                .unwrap()
                .clone()
                .into_iter()
                .zip(annotations.clone().into_iter()),
        );

        for t in &mut self.inputs {
            *t = t.assign(token, &map, types)?;
        }

        for t in &mut self.outputs {
            *t = t.assign(token, &map, types)?;
        }

        self.generics = None;

        Ok(())
    }
}

impl std::fmt::Debug for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "inputs: {:?} outputs: {:?}",
            self.inputs.iter().map(|t| &t.0).collect::<Vec<&String>>(),
            self.outputs.iter().map(|t| &t.0).collect::<Vec<&String>>(),
        )
    }
}
