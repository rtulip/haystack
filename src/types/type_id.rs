use crate::ast::arg::TypedArg;
use crate::ast::member::TypedMember;
use crate::error::HayError;
use crate::lex::token::{Loc, Token, TokenKind, TypeToken};
use std::collections::HashMap;
use std::hash::Hash;

use super::{
    check_requirements, InterfaceBaseType, InterfaceInstanceType, RecordKind, Type, TypeMap,
    UncheckedFunction,
};

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

    pub fn ptr_of(self, mutable: bool, types: &mut TypeMap) -> Self {
        let ptr_typ = Type::Pointer {
            inner: self,
            mutable,
        };

        let ptr_tid = ptr_typ.id();
        types.insert(ptr_typ.id(), ptr_typ);
        ptr_tid
    }

    // Checks to see if a TypeId is generic.
    pub fn is_generic(&self, types: &TypeMap) -> bool {
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
                | Type::Never
                | Type::InterfaceInstance(_),
            ) => false,
            Some(Type::Pointer { inner, .. }) => inner.is_generic(types),
            Some(
                Type::GenericRecordBase { .. }
                | Type::GenericRecordInstance { .. }
                | Type::GenericFunction { .. }
                | Type::InterfaceBase(_),
            )
            | None => true,
            Some(Type::RecordPreDeclaration { generics, .. }) => !generics.is_empty(),
            Some(Type::Stub { .. }) => unimplemented!(),
        }
    }

    /// Creates a new TypeId from a [`Token`]
    ///
    /// Note: panics if `token.kind` is not a [`TypeToken`] and the `token.lexeme`
    /// is not found in either `types` or `local_types`.  
    pub fn from_token(
        token: &Token,
        types: &mut TypeMap,
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
        types: &mut TypeMap,
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
                        ..
                    }) => {
                        
                        // Make sure there are right number of annotations.
                        if generics.len() != inner.len() {
                            return Err(HayError::new(format!("Incorrect number of type annotations provided. Expected annotations for {generics:?}", ), token.loc.clone()));
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

                        // If no annotations are generic, then assign types accordingly.
                        let map: HashMap<TypeId, TypeId> =
                            generics.into_iter().zip(annotations.clone().into_iter()).collect();
                        
                        base_tid.assign(token, &map, types)
                    
                    }
                    Some(
                        Type::InterfaceBase(InterfaceBaseType { name, .. })
                        | Type::InterfaceInstance(InterfaceInstanceType { token: name, .. }),
                    ) => {
                        return Err(HayError::new(
                            format!("Cannot create an instance of interface `{}`", name.lexeme),
                            token.loc.clone(),
                        )
                        .with_hint("Consider adding a `requires` block."))
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
                    Some(Type::Stub { .. }) => unimplemented!(),
                    Some(Type::Never) => unreachable!("Never types aren't representable"),
                    Some(Type::RecordPreDeclaration { .. }) => {
                        unreachable!("Pre declarations aren't allowed past parsing.")
                    }
                    None => Err(HayError::new(
                        format!("Unrecognized base type: {base}"),
                        token.loc.clone(),
                    )),
                }
            }
            TypeToken::Pointer { inner, mutable } => {
                // Get the TypeId of the inner type to create the pointer type.
                let inner_typ_id = TypeId::from_type_token(token, inner, types, local_types)?;
                Ok(inner_typ_id.ptr_of(*mutable, types))
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
        types: &mut TypeMap,
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
                    requires,
                } => {
                    
                    if let Some(requirements) = &requires {
                        match check_requirements(token, requirements, types, map) {
                            Err((Some(r), e)) => return Err(HayError::new(
                                format!(
                                    "Cannot assign {:?} to {kind} `{self}`, as requirements would not be met.",
                                    generics.iter().map(|t| map.get(t).unwrap()).collect::<Vec<&TypeId>>(),
                                ), 
                                token.loc.clone())
                                .with_hint(
                                    format!("{} `{self}` requires `{}` is implemented", 
                                        match kind {
                                            RecordKind::Struct => "Struct",
                                            RecordKind::Union => "Union",
                                            _ => unreachable!()
                                        },
                                        r.lexeme
                                    )
                                )
                                .with_hint(e.message())
                            ),
                            Err((_, e)) => return Err(e),
                            _ => (),
                        }
                    }

                    // Assign each generic
                    let mut resolved_generics = vec![];
                    for t in &generics {
                        resolved_generics.push(t.clone().assign(token, map, types)?);
                    }

                    if resolved_generics.iter().any(|t| t.is_generic(types)) {
                        let t = Type::GenericRecordInstance { base:  self.clone(), base_generics: generics, alias_list: resolved_generics, members, kind };

                        let tid = t.id();
                        types.insert(tid.clone(), t);
                        return Ok(tid);
                    }

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
                    // like: {"T" : "u64"} and {"T": "char"} respectively in our example.
                    //
                    // Now that we have properly addressed the generics aliasing, members
                    // can be resolved, a name generated, and the new record can be created.

                    let mut new_alias_list = vec![];
                    for t in &alias_list {
                        new_alias_list.push(t.assign(token, map, types)?);
                    }

                    if new_alias_list.iter().any(|t| t.is_generic(types)) {
                        let t = Type::GenericRecordInstance { 
                            base, 
                            base_generics, 
                            alias_list: new_alias_list, 
                            members, 
                            kind
                        };

                        let new_id = t.id();
                        types.insert(new_id.clone(), t);
                        return Ok(new_id);
                    }

                    let aliased_generics: HashMap<TypeId, TypeId> = HashMap::from_iter(
                        base_generics
                            .clone()
                            .into_iter()
                            .zip(new_alias_list.into_iter()),
                    );

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
                    Ok(inner.ptr_of(mutable, types))
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
                        || map.len() < func.generics.len()
                    {
                        return Err(HayError::new(
                            "Bad mapping to monomporphize funcion",
                            token.loc.clone(),
                        )
                        .with_hint(format!(
                            "Generic function {} is generic over {:?}.",
                            func.name.lexeme, func.generics
                        ))
                        .with_hint(format!("Found mapping: {map:?}")));
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
                            mutable: input.mutable,
                            ident: input.ident,
                            typ: input.typ.assign(token, map, types)?,
                        });
                    }

                    let mut assigned_outputs = vec![];
                    for output in func.outputs {
                        assigned_outputs.push(TypedArg {
                            token: output.token,
                            mutable: output.mutable,
                            ident: output.ident,
                            typ: output.typ.assign(token, map, types)?,
                        });
                    }

                    if func.requires.is_some() {
                        match check_requirements(
                            &func.name,
                            func.requires.as_ref().unwrap(),
                            types,
                            map,
                        ) {
                            Err((Some(r), e)) => {
                                return Err(HayError::new(
                                    format!(
                                        "Cannot call function `{}` with inputs {:?}, as requirements are not met.", 
                                        func.name.lexeme, 
                                        assigned_inputs
                                            .iter()
                                            .map(|arg| &arg.typ).collect::<Vec<&TypeId>>()
                                    ), 
                                    token.loc.clone())
                                    .with_hint(format!("Function `{}` requires `{}` is implemented", func.name.lexeme, r.lexeme))
                                    .with_hint(e.message())
                                )
                            },
                            Err((_, e)) => return Err(e),
                            _ => (),
                        }
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
                Type::Stub { .. } => unimplemented!(),
                Type::InterfaceBase(_) => unimplemented!(),
                Type::InterfaceInstance(_) => unimplemented!(),
                Type::UncheckedFunction { .. } | Type::Function { .. } => {
                    unreachable!("Should never assign to non-generic function!")
                }
                Type::Never => unreachable!("Never types should never be assigned!"),
                Type::RecordPreDeclaration { .. } => {
                    unreachable!("Pre-declarations should never be assigned")
                }
            },

            None => {
                if !map.contains_key(self) {
                    return Err(HayError::new_type_err(
                        format!("Expected to find {self} in {map:?}"),
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
        types: &mut TypeMap,
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
                    .with_hint(format!("{map:?}")));
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
            ) => Ok(inner
                .resolve(token, &inner_concrete, map, types)?
                .ptr_of(inner_mutable || mutable, types)),
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
            (Some(Type::InterfaceBase(_)), _) => unimplemented!(),
            (Some(Type::InterfaceInstance(_)), _) => unimplemented!(),
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
                    | Type::Function { .. }
                    | Type::Stub { .. },
                ),
                _,
            ) => unreachable!("Functions should never be part of type resolution."),
            (Some(Type::Never), _) => {
                unreachable!("Never types should not be part of type resolution.")
            }
            (Some(Type::RecordPreDeclaration { .. }), _) => {
                unreachable!("Pre-Declarations types should not be part of type resolution.")
            }
        }
    }

    /// Gets the size of a type in bytes.
    pub fn size(&self, types: &TypeMap) -> Result<usize, HayError> {
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
                RecordKind::Interface => unreachable!(),
            },
            Type::InterfaceBase(_) => Err(HayError::new(
                "InterfaceBase types do not have a size",
                Loc::new("", 0, 0, 0),
            )),
            Type::InterfaceInstance(_) => Err(HayError::new(
                "InterfaceInstance types do not have a size",
                Loc::new("", 0, 0, 0),
            )),
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
            | Type::Function { .. }
            | Type::Stub { .. } => Err(HayError::new(
                "Functions do not have a size",
                Loc::new("", 0, 0, 0),
            )),
            Type::RecordPreDeclaration { .. } => Err(HayError::new(
                "Pre-Declared types does not have a size",
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

    pub fn get_inner_accessors<'a>(
        &'a self,
        token: Token,
        inner: &Vec<Token>,
        func: &UncheckedFunction,
        types: &'a TypeMap,
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
                                RecordKind::Interface => unreachable!(),
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

    pub fn validate_redeclaration(
        &self,
        token: &Token,
        pre_decl: (&RecordKind, &Token, &Vec<TypeId>),
        decl: (&RecordKind, &Token, Option<&Vec<TypeId>>),
    ) -> Result<(), HayError> {
        let (pre_decl_kind, pre_decl_token, pre_decl_generics) = pre_decl;
        let (decl_kind, decl_token, decl_generics) = decl;
        if decl_kind != pre_decl_kind {
            return Err(HayError::new(
                "Type Declaration doesn't match Pre-Declaration.",
                token.loc.clone(),
            )
            .with_hint_and_custom_note(
                format!("Type {self} was predeclared as a {pre_decl_kind}",),
                format!("{}", pre_decl_token.loc),
            )
            .with_hint_and_custom_note(
                format!("Type {self} was defined as a {decl_kind}",),
                format!("{}", decl_token.loc),
            ));
        }

        match (pre_decl_generics.len(), decl_generics) {
            (0, Some(generics)) => {
                return Err(HayError::new(
                    "Type Declaration doesn't match Pre-Declaration.",
                    token.loc.clone(),
                )
                .with_hint_and_custom_note(
                    format!("Type {self} was not predeclared as generic",),
                    format!("{}", pre_decl_token.loc.clone()),
                )
                .with_hint_and_custom_note(
                    format!("Type {self} was defined as generic over {generics:?}"),
                    format!("{}", decl_token.loc.clone()),
                ));
            }
            (n, Some(generics)) => {
                if n != generics.len() {
                    return Err(HayError::new(
                        "Type Declaration doesn't match Pre-Declaration.",
                        token.loc.clone(),
                    )
                    .with_hint_and_custom_note(
                        format!(
                            "Type {self} was predeclared as generic over {pre_decl_generics:?}",
                        ),
                        format!("{}", pre_decl_token.loc.clone()),
                    )
                    .with_hint_and_custom_note(
                        format!("Type {self} was not defined as generic over {generics:?}",),
                        format!("{}", decl_token.loc.clone()),
                    ));
                }
            }
            (n, None) if n > 0 => {
                return Err(HayError::new(
                    "Type Declaration doesn't match Pre-Declaration.",
                    token.loc.clone(),
                )
                .with_hint_and_custom_note(
                    format!("Type {self} was predeclared as generic over {pre_decl_generics:?}",),
                    format!("{}", pre_decl_token.loc.clone()),
                )
                .with_hint_and_custom_note(
                    format!("Type {self} was not defined as generic"),
                    format!("{}", token.loc.clone()),
                ));
            }
            _ => (),
        }

        Ok(())
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
