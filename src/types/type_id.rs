use crate::ast::arg::TypedArg;
use crate::ast::member::TypedMember;
use crate::error::HayError;
use crate::lex::token::{Loc, Token, TokenKind, TypeToken, Literal};
use crate::types::{TypeMap, Type, interface::{InterfaceBaseType, check_requirements, InterfaceInstanceType}, RecordKind, UncheckedFunction};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use super::VariantType;

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
            Some(Type::Tuple { inner, .. }) => {
                inner.iter().any(|t| t.is_generic(types))
            }
            Some(Type::Pointer { inner, .. }) => inner.is_generic(types),
            Some(
                Type::GenericRecordBase { .. }
                | Type::GenericRecordInstance { .. }
                | Type::GenericFunction { .. }
                | Type::AssociatedTypeBase(_)
                | Type::InterfaceBase(_),
                
            )
            | None => true,
            Some(Type::RecordPreDeclaration { generics, .. }) => !generics.is_empty(),
            Some(Type::Stub { .. }) => unimplemented!(),
            Some(Type::AssociatedTypeInstance(instance)) => instance.is_generic(types),
            Some(Type::Variant(VariantType { base, .. })) => base.is_generic(types),
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
            TypeToken::Associated { base, typ } => {
                match &**base {
                    TypeToken::Parameterized { base, inner } => {
                        let base_tid = TypeId::new(base);
                        let mut annotations = vec![];
                        for t in inner {
                            annotations.push(TypeId::from_type_token(
                                token,
                                t,
                                types,
                                local_types,
                            )?);
                        }
                        
                        if let Some(base) = base_tid.get_interface_base(types) {
                            let at = base.associated_type_id(token, &TypeId::new(typ))?;
                            let map = base.annotations.clone().into_iter().zip(annotations.clone().into_iter()).collect();
                            at.assign(token, &map, types)
                        } else if let Some(Type::GenericRecordBase { kind: RecordKind::EnumStruct, generics, .. }) = types.get(&base_tid) {
                            let map = generics.clone().into_iter().zip(annotations.clone().into_iter()).collect();

                            let enum_struct_base = base_tid.assign(token, &map, types)?;

                            let t = Type::Variant(VariantType { base: enum_struct_base, variant: typ.clone() });
                            let tid = t.id();
                            types.insert(tid.clone(), t);
                            Ok(tid)
                        } else {
                            return Err(HayError::new(format!("Unknown interface `{base_tid}`"), token.loc.clone()))
                        }                        
                    }
                    TypeToken::Base(base) => {
                        let base_tid = TypeId::new(base);
                        match types.get(&base_tid) {
                            Some(Type::Enum { variants, .. }) => {

                                match variants.iter().find(|tok| &tok.lexeme == typ) {
                                    Some(_) => {
                                        let variant = Type::Variant(VariantType { base: base_tid, variant: typ.clone() });
                                        let tid = variant.id();
                                        types.insert(tid.clone(), variant);
                                        Ok(tid)
                                    },
                                    None => Err(HayError::new(format!("Unknown variant {typ} for enum {base_tid}"), token.loc.clone())),
                                }

                            },
                            Some(Type::Record { members, kind: RecordKind::EnumStruct, .. }) => {

                                match members.iter().find(|m| &m.ident.lexeme == typ) {
                                    Some(_) => {
                                        let variant = Type::Variant(VariantType { base: base_tid, variant: typ.clone() });
                                        let tid = variant.id();
                                        types.insert(tid.clone(), variant);
                                        Ok(tid)
                                    },
                                    None => Err(HayError::new(format!("Unknown variant {typ} for {} {base_tid}", RecordKind::EnumStruct), token.loc.clone())),
                                }

                            },
                            Some(_) => Err(HayError::new(format!("Can't create a variant type from {base_tid}"), token.loc.clone())),
                            None => Err(HayError::new(format!("Can't create variant type from unknown type: {base_tid}"), token.loc.clone())),
                        }

                    },
                    other => panic!("Expected either a Parameterized or Base TypeToken, but found {other:?}"),
                }
            },
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
                        Err(HayError::new(
                            format!("Cannot create an instance of interface `{}`", name.lexeme),
                            token.loc.clone(),
                        )
                        .with_hint("Consider adding a `requires` block."))
                    }
                    Some(Type::Variant(_)) => unreachable!(),
                    Some(
                        Type::Bool
                        | Type::Char
                        | Type::U8
                        | Type::U64
                        | Type::Enum { .. }
                        | Type::Pointer { .. }
                        | Type::Tuple { ..  }
                        | Type::Function { .. }
                        | Type::GenericFunction { .. }
                        | Type::GenericRecordInstance { .. }
                        | Type::Record { .. }
                        | Type::AssociatedTypeBase(_)
                        | Type::AssociatedTypeInstance(_)
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
            TypeToken::Tuple { inner, idents } => {
                
                let mut typs = vec![];
                for t in inner {
                    typs.push(TypeId::from_token(t, types, local_types)?);
                }
                let t = Type::Tuple { inner: typs, idents: idents.clone() };

                let tid= t.id();
                types.insert(t.id(), t);
                Ok(tid)
            },
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

                    if matches!(kind, RecordKind::EnumStruct) {
                        for m in &resolved_members {
                            let t = Type::Variant(VariantType { base: name.clone(), variant: m.ident.lexeme.clone() });
                            types.insert(t.id(), t);
                        }
                    }

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
                        parent: Some(self.clone()),
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
                        members: resolved_members.clone(),
                        kind,
                        parent: Some(base),
                    };

                    let tid = t.id();

                    if matches!(kind, RecordKind::EnumStruct) {
                        for m in &resolved_members {
                            let t = Type::Variant(VariantType { base: tid.clone(), variant: m.ident.lexeme.clone() });
                            types.insert(t.id(), t);
                        }
                    }

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
                Type::Tuple { inner, idents } => {

                    let mut assigned_inner = vec![];

                    for t in inner {
                        assigned_inner.push(t.assign(token, map, types)?);
                    }

                    let tuple = Type::Tuple { inner: assigned_inner, idents };
                    let tid = tuple.id();

                    types.insert(tid.clone(), tuple);

                    Ok(tid)
                },
                Type::Variant(VariantType { base, variant }) => {
                    if self.is_generic(types) {
                        let new_base = base.assign(token, map, types)?;
                        let new_variant = Type::Variant(VariantType{ base: new_base, variant});
                        Ok(new_variant.id())
                    } else {
                        Ok(self.clone())
                    }
                },
                Type::Stub { .. } => unimplemented!(),
                Type::InterfaceBase(_) =>  unimplemented!(),
                Type::InterfaceInstance(_) => unimplemented!(),
                Type::AssociatedTypeBase(at_base) => at_base.assign(token, map, types),
                Type::AssociatedTypeInstance(at_instance) => at_instance.assign(token, map, types),
                Type::UncheckedFunction { .. } | Type::Function { .. } => {
                    unreachable!("Should never assign to non-generic function!")
                }
                Type::Never => unreachable!("Never types should never be assigned!"),
                Type::RecordPreDeclaration { .. } => {
                    Ok(self.clone())
                    // unreachable!("{token}: Pre-declarations should never be assigned -- {self}")
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
                        let prev_st = prev.supertype(types);
                        let concrete_st = concrete.supertype(types);

                        if prev_st != concrete_st {
                            return Err(HayError::new_type_err(
                                "Conflict in type resolution",
                                token.loc.clone(),
                            )
                            .with_hint(format!("Failed to resolve generic type {self}"))
                            .with_hint(format!("Tried to resolve to both {prev} and {concrete}")));    
                        }
                        
                        
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
            (
                Some(Type::Enum { name, .. }),
                Some(Type::Variant(VariantType { base, .. })),
            ) => {
                // Make sure enums are of the same type.
                if name.lexeme != base.0 {
                    return Err(HayError::new(
                        format!(
                            "Failed to resolve enum type `{}` from `{}`",
                            name.lexeme, base.0
                        ),
                        token.loc.clone(),
                    ));
                }

                Ok(concrete.clone())
            }
            (Some(Type::Tuple { inner, idents }), Some(Type::Tuple { inner: concrete_inner, idents: concrete_idents })) => {

                if inner.len() != concrete_inner.len() {
                    return Err(HayError::new(format!("Cannot resolve {self} from {concrete}"), token.loc.clone()));
                }

                match (idents, concrete_idents) {
                    (Some(idents), Some(concrete_idents)) if idents.iter().zip(concrete_idents.iter()).any(|(i, c)| {
                        i.lexeme != c.lexeme
                    }) => return Err(HayError::new(format!("Cannot resolve {self} from {concrete}"), token.loc.clone())),
                    _ => (),    
                }

                for (t, c) in inner.into_iter().zip(concrete_inner.iter()) {
                    t.resolve(token, c, map, types)?;
                }                

                Ok(concrete.clone())
            },
            (Some(Type::Tuple { .. }), _) => {
                Err(HayError::new(format!("Cannot resolve {self} from {concrete}"), token.loc.clone()))
            },
            (
                Some(Type::Variant(VariantType { base, variant })), 
                Some(Type::Variant(VariantType { base: concrete_base, variant: concrete_variant }))
            ) => {

                base.resolve(token, &concrete_base, map, types)?;
                if variant != concrete_variant {
                    return Err(HayError::new(format!("Cannot resolve variant `{self}` from `{concrete}`"), token.loc.clone()));
                }

                Ok(concrete.clone())
            },
            (
                Some(Type::GenericRecordInstance { 
                    kind: RecordKind::EnumStruct, 
                    .. 
                }), 
                Some(Type::Variant(VariantType { base: ref concrete_base, .. }))) => {
                    let instance_base = self.resolve(token, concrete_base, map, types)?;
                    
                    if &instance_base != concrete_base {
                        todo!("Err: Instance doesn't match");
                    }
                    Ok(concrete.clone())
                },
            (Some(Type::InterfaceBase(_)), _) => unimplemented!(),
            (Some(Type::InterfaceInstance(_)), _) => unimplemented!(),
            (Some(Type::AssociatedTypeBase(_)), _) => unimplemented!(),
            (Some(Type::AssociatedTypeInstance(_)), _) => unimplemented!(),
            // Cover all the cases of mismatched types.
            (Some(Type::Pointer { .. }), _)
            | (Some(Type::Bool), _)
            | (Some(Type::Char), _)
            | (Some(Type::U8), _)
            | (Some(Type::U64), _)
            | (Some(Type::Enum { .. }), _)
            | (Some(Type::GenericRecordInstance { .. }), _)
            | (Some(Type::Record { .. }), _)
            | (Some(Type::Variant(_)), _)  => Err(HayError::new_type_err(
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

    pub fn supertype(&self, types: &TypeMap) -> Self {
        match types.get(self) {
            Some(Type::Variant(VariantType { ref base, .. })) => base.clone(),
            Some(Type::Pointer { ref inner, mutable: true, }) => {
                let t = Type::Pointer {
                    inner: inner.clone(),
                    mutable: false
                };
                t.id()
            },
            Some(Type::Record { parent: Some(parent), .. }) => parent.clone(),
            Some(Type::GenericRecordInstance { base, kind: RecordKind::EnumStruct, .. }) => base.clone(),
            Some(_) => self.clone(),
            None => todo!("Not sure what should happen here... {self}"),
        }
    }

    pub fn check_recursive(&self, token: &Token, types: &TypeMap, visited: &mut HashSet<TypeId>) -> Result<(), HayError> {
        if !visited.insert(self.clone()) {
            return Err(HayError::new(format!("Type {self} is recursive."), token.loc.clone())
            .with_hint("Consider adding some kind of indirection, such as a reference."))
        }
        let result = match types.get(self) {
            Some(Type::Bool
            | Type::Char
            | Type::U64
            | Type::U8
            | Type::Enum { .. }
            | Type::Pointer { .. }
            | Type::Tuple { .. }) => Ok(()),
            Some(Type::Record { members, .. } 
            | Type::GenericRecordBase { members, .. } 
            | Type::GenericRecordInstance { members, .. }) => {
                for member in members {
                    member.typ.check_recursive(token, types, visited)?;
                }
                Ok(())
            },
            Some(Type::Variant(_)) => Err(HayError::new(
                "Variant types shouldn't be checked for being recursive",
                token.loc.clone(),
            )),
            Some(Type::InterfaceBase(_)) => Err(HayError::new(
                "InterfaceBase types shouldn't be checked for being recursive",
                token.loc.clone(),
            )),
            Some(Type::InterfaceInstance(_)) => Err(HayError::new(
                "InterfaceInstance types shouldn't be checked for being recursive",
                token.loc.clone()
            )),
            Some(Type::AssociatedTypeBase(_)) => Err(HayError::new(
                "AssociatedTypeBase types shouldn't be checked for being recursive",
                token.loc.clone()
            )),
            Some(Type::AssociatedTypeInstance(_)) => unimplemented!(),
            Some(Type::Never) => Err(HayError::new(
                "Never type shouldn't be checked for being recursive",
                token.loc.clone()
            )),
            Some(Type::UncheckedFunction { .. }
            | Type::GenericFunction { .. }
            | Type::Function { .. }
            | Type::Stub { .. }) => Err(HayError::new(
                "Functions shouldn't be checked for being recursive",
                token.loc.clone()
            )),
            Some(Type::RecordPreDeclaration { .. }) => Err(HayError::new(
                "Pre-Declared shouldn't be checked for being recursive",
                token.loc.clone()
            )),
            None => Ok(())
        };

        visited.remove(self);

        result
    
    }

    /// Gets the size of a type in bytes.
    pub fn size(&self, types: &TypeMap) -> Result<usize, HayError> {
        match types.get(self).unwrap_or_else(|| panic!("{self} should be a known type in the type system")) {
            Type::Bool
            | Type::Char
            | Type::U64
            | Type::U8
            | Type::Enum { .. }
            | Type::Pointer { .. } => Ok(1),
            Type::Tuple { inner, .. } => {
                let mut sum = 0;
                for t in inner {
                    sum += t.size(types)?;
                }
                Ok(sum)
            }
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
                RecordKind::EnumStruct => {
                    let mut sum = 1; // Size starts at 1 for the discriminator
                    for member in members {
                        sum += member.typ.size(types)?;
                    }
                    Ok(sum)
                },
                RecordKind::Interface => unreachable!(),
                RecordKind::Tuple => unreachable!(),
            },
            Type::Variant(VariantType {base, ..}) => base.size(types),
            Type::InterfaceBase(_) => Err(HayError::new(
                "InterfaceBase types do not have a size",
                Loc::new("", 0, 0, 0),
            )),
            Type::InterfaceInstance(_) => Err(HayError::new(
                "InterfaceInstance types do not have a size",
                Loc::new("", 0, 0, 0),
            )),
            Type::AssociatedTypeBase(_) => Err(HayError::new(
                "AssociatedTypeBase types do not have a size",
                Loc::new("", 0, 0, 0),
            )),
            Type::AssociatedTypeInstance(_) => unimplemented!(),
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
            match types.get(typ).unwrap() {
                Type::Record {
                    members,
                    kind: RecordKind::EnumStruct,
                    ..
                } => {
                    match &inner_member.kind {
                        TokenKind::Literal(Literal::U64(n)) => {
                            if *n as usize >= members.len() {
                                return Err(HayError::new(
                                    format!(
                                        "{n} is out of range for `{typ}`. Expected a value between 0 and {} inclusive.", 
                                        members.len() -1
                                    ), 
                                    token.loc
                                ));
                            }

                            typ = &members[*n as usize].typ;
                        },
                        kind => return Err(
                            HayError::new(
                                format!("Internal Error: Expected a number literal to access into `{typ}`, but found {kind} instead."), 
                                token.loc
                        ))
                    }
                },
                Type::Record {
                    name,
                    members,
                    kind,
                    ..
                } => {
                    if let Some(m) = members
                        .iter()
                        .find(|m| m.ident.lexeme == inner_member.lexeme)
                    {
                        if !m.is_public() {
                            match &func.impl_on {
                                Some(typ) => if m.parent.as_ref().unwrap() != typ {
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
                                    RecordKind::EnumStruct => "Enum struct",
                                    RecordKind::Interface => unreachable!(),
                                    RecordKind::Tuple => unreachable!(),
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
                }
                Type::Tuple { inner: tuple_inner, idents: None } => {

                    match &inner_member.kind {
                        TokenKind::Literal(Literal::U64(n)) => {
                            if *n as usize >= tuple_inner.len() {
                                return Err(HayError::new(
                                    format!(
                                        "{n} is out of range for `{typ}`. Expected a value between 0 and {} inclusive.", 
                                        tuple_inner.len() -1
                                    ), 
                                    token.loc
                                ));
                            }

                            typ = &tuple_inner[*n as usize];
                        },
                        kind => return Err(
                            HayError::new(
                                format!("Expected a number literal to access into `{typ}`, but found {kind} instead."), 
                                token.loc
                        ))
                    }

                },
                Type::Tuple { inner: tuple_inner, idents: Some(idents)} => {

                    match &inner_member.kind {
                        TokenKind::Ident(ident) => {
                            let mut found = false;
                            for (idx, id) in idents.iter().enumerate() {
                                if &id.lexeme == ident {
                                    found = true;
                                    typ = &tuple_inner[idx]; 
                                    break;
                                }
                            }

                            if !found {
                                return Err(
                                    HayError::new(
                                        format!(
                                            "Expected one of {:?} to access into `{typ}`, but found {} instead.", 
                                            idents.iter().map(|tok| &tok.lexeme).collect::<Vec<_>>(),
                                            inner_member.kind,
                                        ), 
                                        token.loc
                                ))
                            }
                            
                        },
                        kind => return Err(
                            HayError::new(
                                format!("Expected one of {:?} to access into `{typ}`, but found {kind} instead.", idents.iter().map(|tok| &tok.lexeme).collect::<Vec<_>>()), 
                                token.loc
                        ))
                    }

                },
                _ => {return Err(HayError::new(
                    format!("Cannot access into non-record type `{typ}`"),
                    token.loc,
                ));}
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

    fn get_interface_base<'a>(&self, types: &'a TypeMap) -> Option<&'a InterfaceBaseType> {
        match types.get(self) {
            Some(Type::InterfaceBase(base)) => Some(base),
            _ => None,
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
