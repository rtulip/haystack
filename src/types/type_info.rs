use crate::ast::arg::TypedArg;
use crate::ast::member::TypedMember;
use crate::ast::stmt::GlobalEnv;
use crate::error::HayError;
use crate::lex::token::{Loc, Token, TokenKind, TypeToken};
use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;

use super::{Function, GenericFunction, TypeMap, UncheckedFunction};

/// Unique Identifier for types
///
/// This is just a wrapper around a string, which is used as an identifier
/// for types. The Haystack compiler __MUST__ ensure that Types are uniquely
/// identifyable from their TypeId.
#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeId(pub String);

impl TypeId {
    /// creates a new TypeId.
    pub fn new<S: Into<String>>(id: S) -> Self {
        TypeId(id.into())
    }

    // Checks to see if a TypeId is generic.
    pub fn is_generic(&self, types: &BTreeMap<TypeId, Type>) -> bool {
        match types.get(self) {
            Some(
                Type::Bool
                | Type::Char
                | Type::Enum { .. }
                | Type::U64
                | Type::U8
                | Type::Function { .. }
                | Type::Record { .. }
                | Type::UncheckedFunction { .. }
                | Type::Never,
            ) => false,
            Some(Type::Pointer { inner, .. }) => inner.is_generic(types),
            Some(
                Type::GenericRecordBase { .. }
                | Type::GenericRecordInstance { .. }
                | Type::GenericFunction { .. },
            )
            | None => true,
        }
    }

    /// Creates a new TypeId from a [`Token`]
    ///
    /// Note: panics if `token.kind` is not a [`TypeToken`] and the `token.lexeme`
    /// is not found in either `types` or `local_types`.  
    pub fn from_token(
        token: &Token,
        types: &mut BTreeMap<TypeId, Type>,
        local_types: &Vec<TypeId>,
    ) -> Result<TypeId, HayError> {
        // Shortcut to get the type if it can be found easily.
        if types.contains_key(&TypeId::new(&token.lexeme))
            || local_types.iter().any(|t| t.0 == token.lexeme)
        {
            return Ok(TypeId(token.lexeme.clone()));
        }

        // Extract the TypeToken kind
        let typ = match &token.kind {
            TokenKind::Type(typ) => typ,
            _ => panic!("Didn't expect this...: {:?}", token.kind),
        };

        // Get the TypeId using the TypeToken.
        TypeId::from_type_token(token, typ, types, local_types)
    }

    /// Creates a new TypeId from a [`TypeToken`].
    pub fn from_type_token(
        token: &Token,
        typ: &TypeToken,
        types: &mut BTreeMap<TypeId, Type>,
        local_types: &Vec<TypeId>,
    ) -> Result<TypeId, HayError> {
        match typ {
            TypeToken::Array { base, .. } => {
                // Get the TypeId for the type of the Array TokenType.
                // For example `u8[100]` has a base TypeId of TypeId("u8").
                let base_tid = TypeId::from_type_token(token, base, types, local_types)?;

                // The `Arr` struct is defined in `prelude.hay` and is generic over T.
                // Map `T` to// The TypeId generated id dep the base TypeId
                let map = HashMap::from([(TypeId::new("T"), base_tid)]);
                // Create a new concrete type of `Arr<base_tid>` by assigning Arr<T> with { T: base_tid}.
                let arr_tid = TypeId::new("Arr").assign(token, &map, types)?;

                Ok(arr_tid)
            }
            TypeToken::Base(base) => {
                if types.contains_key(&TypeId::new(base))
                    || local_types.iter().any(|t| &t.0 == base)
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
                        // Make sure there are right number of annotations.
                        if generics.len() != inner.len() {
                            return Err(HayError::new(format!("Incorrect number of type annotations provided. Expected annotations for {:?}", generics), token.loc.clone()));
                        }

                        // Collect inner types into TypeId's
                        let mut annotations = vec![];
                        for t in inner {
                            annotations.push(TypeId::from_type_token(
                                token,
                                t,
                                types,
                                local_types,
                            )?);
                        }

                        if annotations.iter().any(|t| t.is_generic(types)) {
                            // if any annotation is generic then create a generic record instance.
                            let t = Type::GenericRecordInstance {
                                base: TypeId::new(base),
                                base_generics: generics,
                                alias_list: annotations,
                                members,
                                kind,
                            };
                            let tid = t.id();

                            // Insert the new type into the types map.
                            types.insert(tid.clone(), t);
                            Ok(tid)
                        } else {
                            // If no annotations are generic, then assign types accordingly.
                            let map: HashMap<TypeId, TypeId> =
                                generics.into_iter().zip(annotations.into_iter()).collect();
                            base_tid.assign(token, &map, types)
                        }
                    }
                    Some(
                        Type::Bool
                        | Type::Char
                        | Type::U8
                        | Type::U64
                        | Type::Enum { .. }
                        | Type::Pointer { .. }
                        | Type::Function { .. }
                        | Type::GenericFunction { .. }
                        | Type::GenericRecordInstance { .. }
                        | Type::Record { .. }
                        | Type::UncheckedFunction { .. },
                    ) => Err(HayError::new(
                        format!(
                            "Type {base} cannot be annotated, because it is not a generic record."
                        ),
                        token.loc.clone(),
                    )),
                    Some(Type::Never) => unreachable!("Never types aren't representable"),
                    None => Err(HayError::new(
                        format!("Unrecognized base type: {base}"),
                        token.loc.clone(),
                    )),
                }
            }
            TypeToken::Pointer { inner, mutable } => {
                // Get the TypeId of the inner type to create the pointer type.
                let inner_typ_id = TypeId::from_type_token(token, inner, types, local_types)?;
                let t = Type::Pointer {
                    inner: inner_typ_id,
                    mutable: *mutable,
                };

                let tid = t.id();
                types.insert(tid.clone(), t);

                Ok(tid)
            }
        }
    }

    /// Assigns generics for a TypeId based on a mapping and returns the new TypeId created.
    ///
    /// Consider the following example `Haystack` code:
    /// ```
    /// fn foo(Arr<u64>: arr) -> [T] { ... }
    /// ```
    ///
    /// When parsing this function's arguments, we need to be able to assign the type `u64`
    /// to the generic structure `Arr<T>`. That's what this function does.
    ///
    /// ```
    /// let mut types = HashMap::from([
    ///     (TypeId::new("Arr"), Type::GenericRecord { ... }),
    ///     (Type::U64.id(), Type::U64),
    ///     ...
    /// ]);
    /// let map = HashMap::from([(TypeId::new("T"), Type::U64.id())]);
    /// let tid = TypeId::new("Arr");
    /// let new_tid.assign(&token, map, &mut types)?;
    /// assert_eq!(new_tid, TypeId::from("Arr<u64>"));
    /// ```
    pub fn assign(
        &self,
        token: &Token,
        map: &HashMap<TypeId, TypeId>,
        types: &mut BTreeMap<TypeId, Type>,
    ) -> Result<TypeId, HayError> {
        // If the TypeId is in the map return the concrete type.
        if let Some(new_t) = map.get(self) {
            return Ok(new_t.clone());
        }

        let maybe_typ = types.get(self).cloned();
        match maybe_typ {
            // The TypeId is a known type.
            // Assignment is done based on what kind of Type it is.
            Some(typ) => match typ {
                Type::GenericRecordBase {
                    token: base_token,
                    name: name_token,
                    generics,
                    members,
                    kind,
                    ..
                } => {
                    // Assign each member type from the base.
                    let mut resolved_members = vec![];
                    for m in members {
                        resolved_members.push(TypedMember {
                            parent: m.parent,
                            vis: m.vis,
                            token: m.token,
                            ident: m.ident,
                            typ: m.typ.assign(token, map, types)?,
                        });
                    }

                    // Assign each generic
                    let mut resolved_generics = vec![];
                    for t in generics {
                        resolved_generics.push(t.assign(token, map, types)?);
                    }

                    // Construct the new name.
                    // Type name is of the format:
                    //     {base}<{rg1} {rg2} ... {rgn}> where rg1.. rg2 are the names of the resolved generics.
                    //
                    // Note: This naming convention must adhere to the uniqueness requirements of the Haystack compiler.
                    // i.e. Two different types must not have the same name.
                    // This should be sufficient for that.
                    let mut name = format!("{self}<");
                    for t in &resolved_generics[0..resolved_generics.len() - 1] {
                        name = format!("{name}{t} ");
                    }
                    let name = TypeId::new(format!("{name}{}>", resolved_generics.last().unwrap()));

                    // Construct a new record type.
                    let t = Type::Record {
                        token: base_token,
                        name: Token {
                            kind: name_token.kind,
                            lexeme: name.0.clone(),
                            loc: name_token.loc,
                        },
                        members: resolved_members,
                        kind,
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
                    // Assigning to GenericRecordInstances can be a little bit tricky
                    // because there's a layer of generics between the mapping used
                    // as input, and the base generics. It is for this reason, the
                    // Generic Record Instances have an `alias_list`.
                    //
                    // Consider the following Haystack code:
                    // ```
                    // struct Foo<T> { T: t }
                    // fn bar<A B> { Foo<A>: x Foo<B>: y} { .. }
                    // ```
                    //
                    // The function `bar`'s signature looks like this in the IR:
                    // ```
                    // Signature {
                    //     inputs: vec![
                    //         Type::GenericRecordInstance {
                    //             base: "Foo",
                    //             base_generics: vec!["T"],
                    //             alias_list: vec!["A"],
                    //             members: base_members.clone(),
                    //             ...
                    //         }
                    //         Type::GenericRecordInstance {
                    //             base: "Foo",
                    //             base_generics: vec!["T"],
                    //             alias_list: vec!["B"],
                    //             members: base_members.clone(),
                    //             ...
                    //         }
                    //     ],
                    //     outputs: vec![],
                    //     generics:
                    // }
                    // ```
                    //
                    // During type assignment, a map is provided for the generics,
                    // such as { "A": "u64", "B": "char"}. Since the GenericRecordInstance's
                    // members are copies of the base's, blindly assigning to the members
                    // would not correctly account for the aliasing.
                    //
                    // As such, first an alias map is constructed from the alias_list and
                    // base_generics. In this example, the resulting alias map looks like
                    // this: {"T" : "A"} and {"T": "B"} for the two instances respectively.
                    //
                    // Then we create the map which will be used for assignment. This looks
                    // like: {"T:" : "u64"} and {"T": "char"} respectively in our example.
                    //
                    // Now that we have properly addressed the generics aliasing, members
                    // can be resolved, a name generated, and the new record can be created.

                    let alias_map: HashMap<TypeId, TypeId> = HashMap::from_iter(
                        base_generics
                            .clone()
                            .into_iter()
                            .zip(alias_list.into_iter()),
                    );

                    // Build the mapping which will be used for assigning members.
                    let mut aliased_generics = HashMap::new();
                    for (k, v) in &alias_map {
                        aliased_generics.insert(k.clone(), v.clone().assign(token, map, types)?);
                    }

                    // Resolve each member.
                    let mut resolved_members = vec![];
                    for member in members {
                        resolved_members.push(TypedMember {
                            parent: member.parent,
                            vis: member.vis,
                            token: member.token,
                            ident: member.ident,
                            typ: member.typ.assign(token, &aliased_generics, types)?,
                        });
                    }

                    // Create a list of resolved generics in order of the base generics.
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
                            name,
                            token.loc.file.clone(),
                            token.loc.line,
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
                Type::Pointer { inner, mutable } => {
                    // Assign to the inner type and generate a new type if needed.
                    let inner = inner.assign(token, map, types)?;
                    let t = Type::Pointer { inner, mutable };
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
                Type::GenericFunction { func } => {
                    // During type checking, generic functions will be called and then
                    // will need to be monomorphised. Signature::evaluate() will return
                    // the mapping of geneircs which will be used, thus GenericFunctions
                    // will never need to be resolved.

                    // Make sure that each generic is mapped.
                    if !func.generics.iter().all(|tid| map.contains_key(tid))
                        || map.len() != func.generics.len()
                    {
                        return Err(HayError::new(
                            "Bad mapping to monomporphize funcion",
                            token.loc.clone(),
                        )
                        .with_hint(format!(
                            "Generic function {} is generic over {:?}.",
                            func.name.lexeme, func.generics
                        ))
                        .with_hint(format!("Found mapping: {:?}", map)));
                    }

                    // Generate the new name
                    let mut name_string = format!("{}<", func.name.lexeme);
                    for tid in &func.generics[0..func.generics.len() - 1] {
                        name_string = format!("{name_string}{} ", tid.assign(token, map, types)?);
                    }
                    name_string = format!(
                        "{name_string}{}>",
                        func.generics.last().unwrap().assign(token, map, types)?
                    );

                    // Exit early if the monomorphised function already exists.
                    let tid = TypeId::new(&name_string);
                    if types.contains_key(&tid) {
                        return Ok(tid);
                    }

                    let mut assigned_inputs = vec![];
                    for input in func.inputs {
                        assigned_inputs.push(TypedArg {
                            token: input.token,
                            ident: input.ident,
                            typ: input.typ.assign(token, map, types)?,
                        });
                    }

                    let mut assigned_outputs = vec![];
                    for output in func.outputs {
                        assigned_outputs.push(TypedArg {
                            token: output.token,
                            ident: output.ident,
                            typ: output.typ.assign(token, map, types)?,
                        });
                    }

                    // Create a new unchecked function to make sure it gets type checked.
                    let new_fn = Type::UncheckedFunction {
                        func: UncheckedFunction {
                            token: func.token,
                            name: Token {
                                kind: func.name.kind,
                                lexeme: name_string,
                                loc: func.name.loc,
                            },
                            inputs: assigned_inputs,
                            outputs: assigned_outputs,
                            body: func.body,
                            generic_map: Some(map.clone()),
                            tags: func.tags,
                            impl_on: func.impl_on,
                        },
                    };

                    types.insert(tid.clone(), new_fn);

                    Ok(tid)
                }
                Type::UncheckedFunction { .. } | Type::Function { .. } => {
                    unreachable!("Should never assign to non-generic function!")
                }
                Type::Never => unreachable!("Never types should never be assigned!"),
            },
            None => {
                if !map.contains_key(self) {
                    return Err(HayError::new_type_err(
                        format!("Expected to find {self} in {:?}", map),
                        token.loc.clone(),
                    ));
                }

                Ok(map.get(self).unwrap().clone())
            }
        }
    }

    /// Resolves a potentialy generic type from a concrete type and creates a mapping
    /// of any generics that are found.
    ///
    /// Conider the following `Haystack` code:
    /// ```
    /// fn foo<T>(*T: ptr) { ... }
    /// fn main() {
    ///     var u64: bar
    ///     var bool[100]: baz
    ///
    ///     bar foo     // (1)
    /// }
    /// ```
    ///
    /// When we call foo, we need to make sure it's inputs are correct, but it's
    /// signature is generic. On line (1), the stack has a single `*u64` on it, which
    /// we want to make sure is an appropriate argument for `foo`. That's what this
    /// function does.
    ///
    /// ```
    /// let mut types = HashMap::from([
    ///     (TypeId::new("*T"), Type::Pointer{ inner: TypeId::new("T") })
    ///     (TypeId::new("*u64"), Type::Pointer{ inner: TypeId::new("u64") })
    ///     (Type::U64.id(), Type::U64),
    ///     ...
    /// ]);
    /// let mut map = HashMap::new();
    /// let gen_t = TypeId::new("*T"),
    /// let concrete_t = TypeId::new("*u64");
    /// let new_t = gen_t.resolve(token, &concrete_t, &mut map, &mut types)?;
    /// assert_eq!(new_t, concrete_t);
    /// assert_eq!(map.get(&TypeId::new("T")).unwrap(), &Type::U64.id());
    /// ```
    pub fn resolve(
        &self,
        token: &Token,
        concrete: &Self,
        map: &mut HashMap<Self, Self>,
        types: &mut BTreeMap<Self, Type>,
    ) -> Result<Self, HayError> {
        match (types.get(self).cloned(), types.get(concrete).cloned()) {
            (None, None) => {
                // Two types not found in map. i.e. both are placeholder types.

                // Make sure they are the same placeholder.
                if self != concrete {
                    return Err(HayError::new(
                        format!("Cannot resolve generic type {self} from {concrete}"),
                        token.loc.clone(),
                    ));
                }

                // Makesure the map contains te placeholder's value.
                if !map.contains_key(concrete) {
                    return Err(HayError::new(
                        format!("Generic type {self} has not been mapped to a concrete type."),
                        token.loc.clone(),
                    )
                    .with_hint("The following types have been mapped:")
                    .with_hint(format!("{:?}", map)));
                }

                // Return the mapped value.
                Ok(map.get(self).unwrap().clone())
            }
            (None, Some(_)) => {
                // Self is a placeholder and concrete is a value.

                // Map the placeholder to the concrete value & check for collisions.
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
                Some(Type::Pointer { inner, mutable }),
                Some(Type::Pointer {
                    inner: inner_concrete,
                    mutable: inner_mutable,
                }),
            ) => {
                // Resolve the pointer's inner type.
                let p = Type::Pointer {
                    inner: inner.resolve(token, &inner_concrete, map, types)?,
                    mutable: inner_mutable || mutable,
                };

                let tid = p.id();
                types.insert(tid.clone(), p);
                Ok(tid)
            }
            (Some(Type::Record { kind, .. }), Some(Type::Record { .. })) => {
                // Make sure it's the same record.
                if self != concrete {
                    return Err(HayError::new_type_err(
                        format!("Cannot resolve {kind} `{self}` from `{concrete}`."),
                        token.loc.clone(),
                    ));
                }

                Ok(self.clone())
            }
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
                // Can resolve a generic record instance from a concrete record, if and only if
                // the concrete record is derived from the same base.

                if !name.lexeme.starts_with(&base.0) {
                    return Err(HayError::new(
                        format!("Cannot resolve {kind} `{base}` from `{}`", name.lexeme),
                        token.loc.clone(),
                    ));
                }

                assert!(members.len() == generic_members.len());

                // resolve each member from the concrete record's members.
                for (generic, concrete) in generic_members.iter().zip(members) {
                    generic.typ.resolve(token, &concrete.typ, map, types)?;
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
                // Make sure enums are of the same type.
                if name.lexeme != concrete_name.lexeme {
                    return Err(HayError::new(
                        format!(
                            "Failed to resolve enum type `{}` from `{}`",
                            name.lexeme, concrete_name.lexeme
                        ),
                        token.loc.clone(),
                    ));
                }

                Ok(concrete.clone())
            }

            // Cover all the cases of mismatched types.
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
            (
                Some(
                    Type::UncheckedFunction { .. }
                    | Type::GenericFunction { .. }
                    | Type::Function { .. },
                ),
                _,
            ) => unreachable!("Functions should never be part of type resolution."),
            (Some(Type::Never), _) => {
                unreachable!("Never types should not be part of type resolution.")
            }
        }
    }

    /// Gets the size of a type in bytes.
    pub fn size(&self, types: &BTreeMap<TypeId, Type>) -> Result<usize, HayError> {
        match types.get(self).unwrap() {
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
                        sum += member.typ.size(types)?;
                    }
                    Ok(sum)
                }
                RecordKind::Union => {
                    let mut max = 0;
                    for member in members {
                        let sz = member.typ.size(types)?;
                        if sz > max {
                            max = sz;
                        }
                    }

                    Ok(max)
                }
            },
            Type::Never => Err(HayError::new(
                "Never type does not have a size",
                Loc::new("", 0, 0, 0),
            )),
            Type::GenericRecordBase { .. } | Type::GenericRecordInstance { .. } => {
                Err(HayError::new(
                    "Generic Records do not have a size known at compile time",
                    Loc::new("", 0, 0, 0),
                ))
            }
            Type::UncheckedFunction { .. }
            | Type::GenericFunction { .. }
            | Type::Function { .. } => Err(HayError::new(
                "Functions do not have a size",
                Loc::new("", 0, 0, 0),
            )),
        }
    }

    pub fn width(&self) -> usize {
        if self == &Type::U8.id() || self == &Type::Char.id() {
            1
        } else {
            8
        }
    }

    pub fn type_check_inner_accessors<'a>(
        &'a self,
        token: Token,
        inner: &Vec<Token>,
        func: &UncheckedFunction,
        types: &'a BTreeMap<TypeId, Type>,
    ) -> Result<Self, HayError> {
        let mut typ = self;

        for inner_member in inner {
            if let Type::Record {
                name,
                members,
                kind,
                ..
            } = types.get(typ).unwrap()
            {
                if let Some(m) = members
                    .iter()
                    .find(|m| m.ident.lexeme == inner_member.lexeme)
                {
                    if !m.is_public() {
                        match &func.impl_on {
                            Some(typ) => if &m.parent != typ {
                                return Err(
                                    HayError::new_type_err(
                                        format!("Cannot access {kind} `{}` member `{}` as it is declared as private.", name.lexeme, m.ident.lexeme), 
                                        token.loc
                                    ).with_hint_and_custom_note(format!("{kind} `{}` declared here", name.lexeme), format!("{}", name.loc))
                                )
                            }
                            _ => return Err(
                                HayError::new_type_err(
                                    format!("Cannot access {kind} `{}` member `{}` as it is declared as private.", name.lexeme, m.ident.lexeme), 
                                    token.loc
                                ).with_hint_and_custom_note(format!("{kind} `{}` declared here", name.lexeme), format!("{}", name.loc))
                            )
                        }
                    }

                    typ = &m.typ;
                } else {
                    return Err(HayError::new_type_err(
                        format!(
                            "{} `{}` doesn't have a member `{}`",
                            match kind {
                                RecordKind::Union => "Union",
                                RecordKind::Struct => "Struct",
                            },
                            name.lexeme,
                            inner_member.lexeme,
                        ),
                        token.loc,
                    )
                    .with_hint(format!(
                        "`{}` has the following members: {:?}",
                        name.lexeme,
                        members
                            .iter()
                            .map(|m| &m.ident.lexeme)
                            .collect::<Vec<&String>>()
                    )));
                }
            } else {
                return Err(HayError::new(
                    format!("Cannot access into non-record type `{typ}`"),
                    token.loc,
                ));
            }
        }
        Ok(typ.clone())
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

/// Representation of the different kinds of records.
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

/// Representation of Types within Haystack.
///
/// Each non-function type has a unique [`TypeId`]. This uniqueness must be upheld by the compiler.
///
/// There are 4 base built-in types: [`Type::U8`], [`Type::U64`], [`Type::Char`], and [`Type::Bool`].
///
/// More complex types can be built from these built-in types.
/// * [`Type::Pointer`] represents a pointer and holds the [`TypeId`] of an inner type.
/// * [`Type::Record`] represents both concrete and monomorphized structs and unions.
/// * [`Type::GenericRecordBase`] represents the definition of generic structs and unions.
/// * [`Type::GenericRecordInstance`] represents the use of a geneirc struct or enum in a type signature or annotation.
/// * [`Type::Enum`] is a basic c-style enumeration
/// * [`Type::GenericFunction`] represents the definition of a function which is generic over some types.
/// * [`Type::UncheckedFunction`] represents functions which have not yet been type-checked. None-must remain before code generation.
/// * [`Type::Function`] represents final type-checked functions.
#[derive(Debug, Clone)]
pub enum Type {
    /// Built-in unsigned 8-bit integer.
    U8,
    /// Built-in unsigned 64 bit integer.
    U64,
    /// Built-in character type.
    Char,
    /// Built-in boolean.
    Bool,
    /// Built-in Never-type for early returns and functions that will never return.
    Never,
    /// Pointer type.
    Pointer {
        /// The type being pointed to.
        inner: TypeId,
        mutable: bool,
    },
    /// Record types represent struct and union.
    Record {
        /// The token of the `struct` or `union` keywords.
        token: Token,
        /// The token containing the name of the record.
        name: Token,
        /// The members of the struct or union.
        /// The compiler MUST guarantee that these types are known within the `types` map during compilation.
        members: Vec<TypedMember>,
        /// A flag to indicate if the record is a struct or union.
        kind: RecordKind,
    },
    /// Type to represent the definition of a generic record.
    GenericRecordBase {
        /// The token of the `struct` or `union` keywords.
        token: Token,
        /// The token containing the name of the record.
        name: Token,
        /// What types the record is generic over.
        /// Note: The order of these types DOES matter for type resolution and assignment.
        /// Note: These types will not appear in the `types` map during compilation.
        generics: Vec<TypeId>,
        /// The members of the struct or union.
        /// These types are allowed to not be present within the `types` map during compilation.
        members: Vec<TypedMember>,
        /// A flag to indicate if the record is a struct or union.
        kind: RecordKind,
    },
    /// Type to represent the instantiation of a generic record
    GenericRecordInstance {
        /// The [`Type::GenericRecordBase`] which this instance originates from.
        base: TypeId,
        /// A copy of the [`Type::GenericRecordBase`]'s generics
        base_generics: Vec<TypeId>,
        /// A list of [`TypeId`] the same length as `base_generics`.
        /// This is used to create a mapping from the base generics types within the local scope.
        alias_list: Vec<TypeId>,
        /// The members of the struct or union.
        /// These types are allowed to not be present within the `types` map during compilation.
        members: Vec<TypedMember>,
        /// A flag to indicate if the record is a struct or union.
        kind: RecordKind,
    },
    /// Representation of a simple c-style enum
    Enum {
        /// Token of the `enum` keyword
        token: Token,
        /// Token containing the name of the enum.
        name: Token,
        /// List of tokens for each variant of the enum.
        variants: Vec<Token>,
    },
    /// Represents a generic function.
    /// Generic Functions are not type checked.
    GenericFunction { func: GenericFunction },
    /// Represents a concrete function that needs to be type checked.
    UncheckedFunction { func: UncheckedFunction },
    /// Represents a function that has been type checked.
    Function { func: Function },
}

impl Type {
    /// Builds the unique [`TypeId`] for a given type.
    pub fn id(&self) -> TypeId {
        match self {
            Type::U64 => TypeId::new("u64"),
            Type::U8 => TypeId::new("u8"),
            Type::Char => TypeId::new("char"),
            Type::Bool => TypeId::new("bool"),
            Type::Never => TypeId::new("!"),
            Type::Enum { name, .. }
            | Type::GenericRecordBase { name, .. }
            | Type::Record { name, .. } => TypeId::new(&name.lexeme),
            Type::Pointer { inner, mutable } => {
                if *mutable {
                    TypeId::new(format!("*{}", inner.0))
                } else {
                    TypeId::new(format!("&{}", inner.0))
                }
            }
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
            Type::UncheckedFunction { .. }
            | Type::GenericFunction { .. }
            | Type::Function { .. } => {
                unimplemented!("Haven't implemented name from Functions.")
            }
        }
    }

    pub fn new_map() -> TypeMap {
        let mut types = BTreeMap::new();

        types.insert(Type::U64.id(), Type::U64);
        types.insert(Type::U8.id(), Type::U8);
        types.insert(Type::Bool.id(), Type::Bool);
        types.insert(Type::Char.id(), Type::Char);
        types.insert(
            TypeId::new("&T"),
            Type::Pointer {
                inner: TypeId::new("T"),
                mutable: false,
            },
        );
        types.insert(
            TypeId::new("*T"),
            Type::Pointer {
                inner: TypeId::new("T"),
                mutable: true,
            },
        );

        types
    }

    pub fn type_check_functions(
        types: &mut TypeMap,
        global_env: &GlobalEnv,
    ) -> Result<(), HayError> {
        while types
            .iter()
            .filter(|(_, v)| matches!(v, Type::UncheckedFunction { .. }))
            .count()
            != 0
        {
            let fns = types
                .drain_filter(|_, v| matches!(v, Type::UncheckedFunction { .. }))
                .collect::<Vec<(TypeId, Type)>>();

            for (tid, f) in fns {
                let func = f.unchecked_function().clone();
                let mut stack = vec![];
                let mut frame = vec![];

                func.inputs.iter().rev().for_each(|arg| {
                    if arg.ident.is_some() {
                        frame.push((arg.ident.as_ref().unwrap().lexeme.clone(), arg.typ.clone()))
                    } else {
                        stack.push(arg.typ.clone())
                    }
                });

                let mut typed_body = vec![];
                for expr in func.body.clone() {
                    typed_body.push(expr.type_check(
                        &mut stack,
                        &mut frame,
                        &func,
                        global_env,
                        types,
                        &func.generic_map,
                    )?);
                }
                let stack_tids = stack.iter().collect::<Vec<&TypeId>>();
                let output_tids = func
                    .outputs
                    .iter()
                    .map(|arg| &arg.typ)
                    .collect::<Vec<&TypeId>>();

                if !stack_tids.contains(&&Type::Never.id()) && stack_tids != output_tids {
                    return Err(HayError::new_type_err(
                        format!(
                            "Function `{}` doesn't produce the correct outputs",
                            func.name.lexeme
                        ),
                        func.name.loc,
                    )
                    .with_hint(format!("Expected final stack: {:?}", output_tids))
                    .with_hint(format!("Function produced:    {:?}", stack_tids)));
                }

                types.insert(
                    tid,
                    Type::Function {
                        func: Function {
                            token: func.token,
                            name: func.name,
                            inputs: func.inputs,
                            outputs: func.outputs,
                            body: typed_body,
                            generic_map: func.generic_map,
                            tags: func.tags,
                        },
                    },
                );
            }
        }

        Ok(())
    }

    pub fn function(&self) -> &Function {
        if let Type::Function { func } = self {
            func
        } else {
            panic!("Tried to extract a function from a non-function type");
        }
    }

    pub fn unchecked_function(&self) -> &UncheckedFunction {
        if let Type::UncheckedFunction { func } = self {
            func
        } else {
            panic!("Tried to extract an unchecked function from a non-unchecked-function type")
        }
    }

    // pub fn generic_function(&self) -> &GenericFunction {
    //     if let Type::GenericFunction { func } = self {
    //         func
    //     } else {
    //         panic!("Tried to extract a generic-function from a non-generic-function type")
    //     }
    // }

    pub fn try_generic_function(&self, token: &Token) -> Result<&GenericFunction, HayError> {
        if let Type::GenericFunction { func } = self {
            Ok(func)
        } else {
            Err(HayError::new(
                "Tried to extract a generic-function from a non-generic-function type",
                token.loc.clone(),
            ))
        }
    }
}

type Predicate = dyn Fn(&Vec<TypeId>, &BTreeMap<TypeId, Type>) -> bool;

/// A Structure to describe changes to the stack.
///
/// During type checking, each [`crate::ast::expr::Expr`] will manipulate the
/// stack. That manipulation is represented with a Signature.
///
/// Signatures can be generic, as to allow for type checking generic function
/// calls.
///
/// Additionally, signatures can have a Predicate, which is a condition that
/// must be true in order for the Signature to evaulate successfully.
///
/// For example the `==` and `!=` operators accept the following signatures:
///
///     1. [u64  u64 ] -> [bool]
///     2. [u8   u8  ] -> [bool]
///     3. [char char] -> [bool]
///     4. [*T   *T  ] -> [bool]
///     5. [E    E   ] -> [bool] where E is an enum.
///
/// The first three are trivial to construct:
/// ```rust
/// let sig = Signature::new(
///     vec![Type::U64.id(), Type::U64.id()],
///     vec![Type::Bool.id()]
/// );
/// ```
///
/// Constructing the 4th requires making a generic signature:
/// ```
/// let generic_sig = Signature::new_generic(
///     vec![TypeId::new("*T"), TypeId::new("*T")]  // Create the TypeId for the generic types
///     vec![Type::Bool.id()]
///     vec![TypeId::new("T")]                      // Include the base generics
/// );
/// ```
///
/// The 5th requires a predicate, which can be added after building the Signature.
/// ```
/// let enums_sig = Signature::new_generic(
///     vec![TypeId::new("E"), TypeId::new("E")],
///     vec![Type::Bool.id()],
///     vec![TypeId::new("E")]
/// ).with_predicate(
///     &|inputs, types| match (types.get(&inputs[0]), types.get(&inputs[1])) {
///         (
///             Some(Type::Enum { name: left, .. }),
///             Some(Type::Enum { name: right, .. }),
///         ) => left.lexeme == right.lexeme,
///         _ => false,
///     },
///     "E is an enum",
/// );
/// ```
#[derive(Clone)]
pub struct Signature<'pred> {
    pub inputs: Vec<TypeId>,
    pub outputs: Vec<TypeId>,
    pub generics: Option<Vec<TypeId>>,
    predicate: Option<(&'pred Predicate, String)>,
}

impl<'pred> Signature<'pred> {
    /// Constructs a new signature.
    /// ```
    /// // [u64 Str] -> [bool]
    /// let sig = Signature::new(
    ///     vec![Type::U64.id(), TypeId::new("Str")],
    ///     vec![Type::Bool.id()]
    /// );
    /// ```
    pub fn new(inputs: Vec<TypeId>, outputs: Vec<TypeId>) -> Self {
        Self {
            inputs,
            outputs,
            generics: None,
            predicate: None,
        }
    }

    /// Construct a new generic signature.
    /// ```
    /// // [*T u64] -> [*T]
    /// let sig = Signature::new(
    ///     vec![TypeId::new("*T"), Type::U64.id()],
    ///     vec![TypeId::new("*T")],
    ///     vec![TypeId::new("T")]  // indicate that sig is generic over `T`
    /// );
    /// ```
    pub fn new_generic(inputs: Vec<TypeId>, outputs: Vec<TypeId>, generics: Vec<TypeId>) -> Self {
        Self {
            inputs,
            outputs,
            generics: Some(generics),
            predicate: None,
        }
    }

    /// Constructs an optionally generic signature.
    pub fn new_maybe_generic(
        inputs: Vec<TypeId>,
        outputs: Vec<TypeId>,
        generics: Option<Vec<TypeId>>,
    ) -> Self {
        Self {
            inputs,
            outputs,
            generics,
            predicate: None,
        }
    }

    /// Adds a predicate to a Signature.
    ///
    /// Note: panics if `self` alreayd has a predicate.
    ///
    /// ```
    /// // [T] -> [] where sizeOf(T) == 1
    /// let sig = Signature::new_generic(
    ///     vec![TypeId::new("T")],
    ///     vec![],
    ///     vec![TypeId::new("T")],
    /// ).with_predicate(
    ///     &|inputs, types| inputs[0].size(types) == 1,
    ///     "sizeOf(T) == 1"
    /// );
    /// ```
    pub fn with_predicate<S>(self, predicate: &'pred Predicate, message: S) -> Self
    where
        S: Into<String>,
    {
        if self.predicate.is_some() {
            panic!("{:?} already had a predicate.", self);
        }

        Self {
            inputs: self.inputs,
            outputs: self.outputs,
            generics: self.generics,
            predicate: Some((predicate, message.into())),
        }
    }

    /// Consumes the top of the stack if it matches the Signature's inputs
    /// then pushes the outputs onto the stack.
    ///
    /// Adds newly monomorphized types into the `types` map.
    ///
    /// Returns a map of assigned generic types if the Signature was generic.
    ///
    /// ```
    /// // The initial stack has a `u64` on it.
    /// let mut stack = vec![Type::U64.id()];
    ///
    /// // [u64] -> [bool]
    /// Signature::new(vec![Type::U64.id()],vec![Type::Bool.id()])
    ///     .evaluate(&token, &mut stack, &mut types);
    /// assert_eq!(&stack, &vec![TypeId::Bool.id()]);
    ///
    /// Signature::new_generic(
    ///     vec![TypeId::new("T")],
    ///     vec![TypeId::new("T"), TypeId::new("T")],
    ///     vec![TypeId::new("T")]
    /// ).evaluate(&token, &mut stack, &mut types);
    /// assert_eq!(&stack, &vec![TypeId::Bool.id(), TypeId::Bool.id()]);
    /// ```
    pub fn evaluate(
        &self,
        token: &Token,
        stack: &mut Vec<TypeId>,
        types: &mut BTreeMap<TypeId, Type>,
    ) -> Result<Option<HashMap<TypeId, TypeId>>, HayError> {
        // Make sure the stack has at least as many elements as the inputs.
        // This ensures that the stack doesn't underflow.
        if stack.len() < self.inputs.len() {
            return Err(HayError::new_type_err(
                format!("Invalid number of inputs for {:?}", token.lexeme),
                token.loc.clone(),
            )
            .with_hint(format!("Expected: {:?}", self.inputs))
            .with_hint(format!("Found:    {:?}", stack)));
        }

        // Need to resolve the generics if `self` is generic.
        // Don't want to copy `self` otherwise.
        let mut map = None;
        let mut to_resolve;
        let sig = if self.generics.is_some() {
            // Need to clone to make `self` mutable for resolution.
            // Don't want to take in &mut self, because most signatures should
            // be constant, such as function signatures
            to_resolve = self.clone();
            map = to_resolve.resolve(token, stack, types)?;
            &to_resolve
        } else {
            self
        };
        // Check that each input matches the element on the stack.
        for (input, stk) in sig.inputs.iter().rev().zip(stack.iter().rev()) {
            if input != stk {
                if let (
                    Some(Type::Pointer {
                        inner: input_inner,
                        mutable: false,
                    }),
                    Some(Type::Pointer {
                        inner: stk_inner,
                        mutable: true,
                    }),
                ) = (types.get(input), types.get(stk))
                {
                    if input_inner == stk_inner {
                        continue;
                    }
                }

                return Err(HayError::new_type_err(
                    format!("Invalid inputs for `{}`", token.lexeme).as_str(),
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

        // If the signature has a predicate, evaulate it.
        if let Some((pred, msg)) = &sig.predicate {
            if !pred(&sig.inputs, types) {
                return Err(HayError::new_type_err(
                    format!("Invalid inputs for {:?}", token.lexeme).as_str(),
                    token.loc.clone(),
                )
                .with_hint(format!("Expected: {:?} where {msg}", sig.inputs))
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

        // Pop the inputs off of the stack, since by this point it's been
        // checked that the inputs matched the stack.
        for _ in &sig.inputs {
            stack.pop();
        }

        // Push the outputs onto the stack.
        for out in &sig.outputs {
            stack.push(out.clone());
        }

        // Return the map that might have been used during type resolution.
        Ok(map)
    }

    /// Evaluates multiple signatures and applies the first which succeeds.
    pub fn evaluate_many(
        sigs: &[Signature],
        token: &Token,
        stack: &mut Vec<TypeId>,
        types: &mut BTreeMap<TypeId, Type>,
    ) -> Result<Option<HashMap<TypeId, TypeId>>, HayError> {
        // Make sure that each signature has the same "shape"
        // This might not be strctly nessisary.
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

        // Evaluate each Signature & return early if successful.
        for sig in sigs {
            if let Ok(x) = sig.evaluate(token, stack, types) {
                return Ok(x);
            }
        }

        // Build a nice error message.
        // If this point is reached, none of the signatures evaluated successfully.
        let mut e = HayError::new_type_err(
            format!("Invalid inputs for {}", token.kind),
            token.loc.clone(),
        )
        .with_hint(format!("Expected one of {} signatures:", sigs.len()));

        for sig in sigs {
            e = e.with_hint(format!(
                "  {:?}{}",
                sig.inputs,
                if let Some((_, msg)) = &sig.predicate {
                    format!(" where {msg}")
                } else {
                    String::new()
                }
            ));
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

    /// Resolves each type in the signature based on the stack.
    fn resolve(
        &mut self,
        token: &Token,
        stack: &mut [TypeId],
        types: &mut BTreeMap<TypeId, Type>,
    ) -> Result<Option<HashMap<TypeId, TypeId>>, HayError> {
        // No work to do if the signature isn't generic.
        if self.generics.is_none() {
            return Ok(None);
        }

        let mut map = HashMap::new();
        let len = self.inputs.len();

        // Resolve each type based on the concrete type found on the stack.
        for (t, concrete) in self
            .inputs
            .iter_mut()
            .zip(stack.iter().rev().take(len).rev())
        {
            *t = t.resolve(token, concrete, &mut map, types)?;
        }

        // Given the generic mapping created from the resolution of the inputs,
        // assign each output accordingly.
        for t in &mut self.outputs {
            *t = t.assign(token, &map, types)?;
        }

        if map.is_empty() {
            Err(HayError::new(
                "Map should be non-empty at this point",
                token.loc.clone(),
            ))
        } else {
            Ok(Some(map))
        }
    }

    /// Assigns a mapping to each type in the signature.
    pub fn assign(
        &mut self,
        token: &Token,
        annotations: &[TypeId],
        types: &mut BTreeMap<TypeId, Type>,
    ) -> Result<(), HayError> {
        // Check that the signature is generic. Doesn't make sense to assign
        // geneircs to a non-geneirc Signature.
        if self.generics.is_none() {
            return Err(HayError::new_type_err(
                "Cannot assign to non-generic signature.",
                token.loc.clone(),
            ));
        }

        // Make sure that the annotations are of the right length.
        if self.generics.as_ref().unwrap().len() != annotations.len() {
            return Err(HayError::new_type_err(
                format!(
                    "Signature expected {} annotations for geneircs: {:?}, but found {:?}",
                    self.generics.as_ref().unwrap().len(),
                    self.generics.as_ref().unwrap(),
                    annotations
                ),
                token.loc.clone(),
            ));
        }

        // Create the map needed by TypeId::assign.
        let map: HashMap<TypeId, TypeId> = HashMap::from_iter(
            self.generics
                .as_ref()
                .unwrap()
                .clone()
                .into_iter()
                .zip(annotations.iter().cloned()),
        );

        // Assign to each input
        for t in &mut self.inputs {
            *t = t.assign(token, &map, types)?;
        }

        // Assign to each output
        for t in &mut self.outputs {
            *t = t.assign(token, &map, types)?;
        }

        // Since the signature has been assigned, it should no longer be generic.
        self.generics = None;

        Ok(())
    }
}

impl<'pred> std::fmt::Debug for Signature<'pred> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} -> {:?}",
            self.inputs.iter().map(|t| &t.0).collect::<Vec<&String>>(),
            self.outputs.iter().map(|t| &t.0).collect::<Vec<&String>>(),
        )?;

        if let Some((_, msg)) = &self.predicate {
            write!(f, " where {msg}")?;
        }

        Ok(())
    }
}

mod tests {

    #[test]
    fn record_record_resolution() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "record_record_resolution")
    }

    #[test]
    fn generic_record_record_resolution() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "generic_record_record_resolution")
    }

    #[test]
    fn enum_enum_resolution() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "enum_enum_resolution")
    }

    #[test]
    fn generic_record_size() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "generic_record_size")
    }

    #[test]
    fn immutable_pointer_write() -> Result<(), std::io::Error> {
        crate::compiler::test_tools::run_test("type_check", "immutable_pointer_write")
    }
}
