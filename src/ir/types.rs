use crate::compiler::compiler_error;
use crate::ir::{token::Token, Stack};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
pub type TypeName = String;

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    U64,
    U8,
    Bool,
    Enum {
        name: TypeName,
        variants: Vec<String>,
    },
    Placeholder {
        name: TypeName,
    },
    Pointer {
        typ: TypeName,
    },
    Struct {
        name: TypeName,
        members: Vec<TypeName>,
        visibility: Vec<Visibility>,
        idents: Vec<String>,
    },
    GenericStructBase {
        name: TypeName,
        members: Vec<TypeName>,
        visibility: Vec<Visibility>,
        idents: Vec<String>,
        generics: Vec<TypeName>,
    },
    GenericStructInstance {
        base: TypeName,
        members: Vec<TypeName>,
        visibility: Vec<Visibility>,
        idents: Vec<String>,
        alias_list: Vec<TypeName>,
        base_generics: Vec<TypeName>,
    },
    ResolvedStruct {
        name: TypeName,
        members: Vec<TypeName>,
        visibility: Vec<Visibility>,
        idents: Vec<String>,
        base: TypeName,
    },
    Union {
        name: TypeName,
        members: Vec<TypeName>,
        visibility: Vec<Visibility>,
        idents: Vec<String>,
    },
    GenericUnionBase {
        name: TypeName,
        members: Vec<TypeName>,
        visibility: Vec<Visibility>,
        idents: Vec<String>,
        generics: Vec<TypeName>,
    },
    GenericUnionInstance {
        base: TypeName,
        members: Vec<TypeName>,
        visibility: Vec<Visibility>,
        idents: Vec<String>,
        alias_list: Vec<TypeName>,
        base_generics: Vec<TypeName>,
    },
    ResolvedUnion {
        name: TypeName,
        members: Vec<TypeName>,
        visibility: Vec<Visibility>,
        idents: Vec<String>,
        base: TypeName,
    },
}

impl Type {
    pub fn name(&self) -> String {
        match self {
            Type::GenericStructBase { name, .. } | Type::GenericUnionBase { name, .. } => {
                name.clone()
            }
            _ => format!("{}", self),
        }
    }

    pub fn is_generic(&self, type_map: &HashMap<TypeName, Type>) -> bool {
        match self {
            Type::Placeholder { .. }
            | Type::GenericStructBase { .. }
            | Type::GenericStructInstance { .. }
            | Type::GenericUnionBase { .. }
            | Type::GenericUnionInstance { .. } => true,
            Type::Pointer { typ } => type_map.get(typ).unwrap().is_generic(type_map),
            Type::U64
            | Type::U8
            | Type::Bool
            | Type::Enum { .. }
            | Type::Struct { .. }
            | Type::ResolvedStruct { .. }
            | Type::Union { .. }
            | Type::ResolvedUnion { .. } => false,
        }
    }

    pub fn deep_check_generics(
        &self,
        type_map: &HashMap<TypeName, Type>,
        visited: &mut HashSet<TypeName>,
    ) -> Vec<TypeName> {
        if visited.insert(self.name()) {
            match self {
                Type::U64 | Type::U8 | Type::Bool | Type::Enum { .. } => {
                    vec![]
                }
                Type::Placeholder { .. } => vec![self.name()],
                Type::Pointer { typ } => type_map
                    .get(typ)
                    .unwrap()
                    .deep_check_generics(type_map, visited),
                Type::Struct { members, .. }
                | Type::Union { members, .. }
                | Type::GenericStructBase { members, .. }
                | Type::GenericStructInstance { members, .. }
                | Type::ResolvedStruct { members, .. }
                | Type::GenericUnionBase { members, .. }
                | Type::GenericUnionInstance { members, .. }
                | Type::ResolvedUnion { members, .. } => {
                    let mut generics = vec![];
                    members.iter().for_each(|m| {
                        let t = type_map.get(m).unwrap();
                        generics.append(&mut t.deep_check_generics(type_map, visited));
                    });
                    generics
                }
            }
        } else {
            vec![]
        }
    }

    pub fn shallow_check_generics(&self, type_map: &HashMap<TypeName, Type>) -> Vec<TypeName> {
        match self {
            Type::U64
            | Type::U8
            | Type::Bool
            | Type::Enum { .. }
            | Type::Struct { .. }
            | Type::Union { .. }
            | Type::ResolvedStruct { .. }
            | Type::ResolvedUnion { .. } => vec![],
            Type::Placeholder { .. } => vec![self.name()],
            Type::Pointer { typ } => type_map.get(typ).unwrap().shallow_check_generics(type_map),
            Type::GenericStructBase { generics, .. }
            | Type::GenericStructInstance {
                alias_list: generics,
                ..
            }
            | Type::GenericUnionBase { generics, .. }
            | Type::GenericUnionInstance {
                alias_list: generics,
                ..
            } => generics.clone(),
        }
    }

    pub fn size(&self, token: &Token, type_map: &HashMap<TypeName, Type>) -> usize {
        match self {
            Type::U64 | Type::U8 | Type::Bool | Type::Pointer { .. } | Type::Enum { .. } => 1,
            Type::Struct { members, .. } | Type::ResolvedStruct { members, .. } => members
                .iter()
                .map(|t| type_map.get(t).unwrap().size(token, type_map))
                .sum(),
            Type::Union { members, .. } | Type::ResolvedUnion { members, .. } => members
                .iter()
                .map(|t| type_map.get(t).unwrap().size(token, type_map))
                .max()
                .unwrap(),
            Type::GenericUnionBase { .. } => {
                compiler_error(
                    token,
                    "Size of generic union base is unkown",
                    vec![format!("Generic Struct base: {self}").as_str()],
                );
            }
            Type::GenericUnionInstance { .. } => compiler_error(
                token,
                "Size of generic union instance is unknown",
                vec![format!("Generic Union Instance: {self}").as_str()],
            ),
            Type::Placeholder { .. } => compiler_error(
                token,
                "Size of Placeholder types are unknown",
                vec![format!("Placeholder type: {self}").as_str()],
            ),
            Type::GenericStructBase { .. } => compiler_error(
                token,
                "Size of Generic Struct Base is unknown",
                vec![format!("Generic Struct Base: {self}").as_str()],
            ),
            Type::GenericStructInstance { .. } => compiler_error(
                token,
                "Size of generic struct instance is unknown",
                vec![format!("Generic Struct Instance: {self}").as_str()],
            ),
        }
    }

    pub fn width(&self) -> usize {
        match self {
            Type::U8 => 1,
            _ => 8,
        }
    }

    pub fn str() -> Self {
        Type::Struct {
            name: String::from("Str"),
            members: vec![
                Type::U64.name(),
                Type::Pointer {
                    typ: Type::U8.name(),
                }
                .name(),
            ],
            visibility: vec![Visibility::Public, Visibility::Public],
            idents: vec![String::from("size"), String::from("data")],
        }
    }

    pub fn arr_base() -> Self {
        Type::GenericStructBase {
            name: String::from("Arr"),
            members: vec![
                Type::U64.name(),
                Type::Pointer {
                    typ: Type::Placeholder {
                        name: String::from("T"),
                    }
                    .name(),
                }
                .name(),
            ],
            visibility: vec![Visibility::Public, Visibility::Public],
            idents: vec![String::from("size"), String::from("data")],
            generics: vec![String::from("T")],
        }
    }

    pub fn resolve_struct(
        token: &Token,
        gen_struct_t: &TypeName,
        stack: &Stack,
        type_map: &mut HashMap<TypeName, Type>,
    ) -> TypeName {
        let (pairs, base, visibility, idents, generics) = match type_map.get(gen_struct_t).unwrap().clone() {
            Type::GenericStructBase {
                name,
                members,
                visibility,
                idents,
                generics,
            } => {
                let pairs: Vec<(TypeName, TypeName)> = members
                    .iter()
                    .zip(stack[stack.len() - members.len()..].iter())
                    .map(|(t1, t2)| (t1.clone(), t2.clone()))
                    .collect();
                (pairs, name, visibility, idents.clone(), generics.clone())
            }
            _ => panic!("Resolve struct should only be run on Base Generic Structs...\n{}: Type: {:?} Stack: {:?}", token.loc, gen_struct_t, stack),
        };

        let mut map: HashMap<TypeName, TypeName> = HashMap::new();
        let resolved_members = pairs
            .iter()
            .map(|(t1, t2)| {
                let mut m: HashMap<TypeName, TypeName> = HashMap::new();
                let t = Type::resolve_type(token, t1, t2, &mut m, type_map);

                m.drain().for_each(|(k, v)| {
                    if let Some(typ) = map.insert(k.clone(), v.clone()) {
                        if typ != v {
                            compiler_error(token, "Type Resolution Failure in generic struct resolution", vec![
                                format!("Type `{k}` cannot be assigned to {v} as it was previously assigned to {typ}").as_str(),
                            ]);
                        }
                    }
                });


                t
            })
            .collect::<Vec<TypeName>>();
        if !generics.iter().all(|t| map.contains_key(t)) {
            compiler_error(
                token,
                "Some types were not resolved during cast",
                vec![format!(
                    "These types were not resolved: {:?}",
                    generics
                        .iter()
                        .filter(|t| !map.contains_key(&format!("{t}")))
                        .collect::<Vec<&TypeName>>()
                )
                .as_str()],
            )
        }

        let mut name = base.clone();
        name.push_str(format!("<{}", map.get(&generics[0]).unwrap()).as_str());
        for t in generics[1..].iter() {
            name.push_str(format!(" {}", map.get(t).unwrap()).as_str());
        }
        name.push('>');

        let t = Type::ResolvedStruct {
            name,
            members: resolved_members,
            visibility,
            idents,
            base: base.clone(),
        };

        type_map.insert(t.name(), t.clone());
        t.name()
    }

    pub fn resolve_type(
        token: &Token,
        maybe_generic_t: &TypeName,
        concrete_t: &TypeName,
        generic_map: &mut HashMap<TypeName, TypeName>,
        type_map: &mut HashMap<TypeName, Type>,
    ) -> TypeName {
        match (
            type_map.get(maybe_generic_t).unwrap().clone(),
            type_map.get(concrete_t).unwrap().clone(),
        ) {
            (Type::U64, Type::U64) => Type::U64.name(),
            (Type::U64, _) => compiler_error(
                token,
                format!(
                    "Cannot resolve type {:?} into {:?}",
                    maybe_generic_t, concrete_t
                )
                .as_str(),
                vec![],
            ),
            (Type::U8, Type::U8) => Type::U8.name(),
            (Type::U8, _) => compiler_error(
                token,
                format!(
                    "Cannot resolve type {:?} into {:?}",
                    maybe_generic_t, concrete_t
                )
                .as_str(),
                vec![],
            ),
            (Type::Bool, Type::Bool) => Type::Bool.name(),
            (Type::Bool, _) => compiler_error(
                token,
                format!(
                    "Cannot resolve type {:?} into {:?}",
                    maybe_generic_t, concrete_t
                )
                .as_str(),
                vec![],
            ),
            (Type::Pointer { typ, .. }, Type::Pointer { typ: typ2, .. }) => {
                let pointer_typ = Type::Pointer {
                    typ: Type::resolve_type(token, &typ, &typ2, generic_map, type_map),
                };
                // todo: Check that this is fine to just insert without checking if it failed.
                type_map.insert(pointer_typ.name(), pointer_typ.clone());
                pointer_typ.name()
            }
            (Type::Pointer { .. }, _) => compiler_error(
                token,
                format!(
                    "Cannot resolve type {:?} into {:?}",
                    maybe_generic_t, concrete_t
                )
                .as_str(),
                vec![],
            ),
            (Type::Placeholder { .. }, Type::GenericStructBase { .. }) => unreachable!(),
            (Type::Placeholder { .. }, Type::GenericStructInstance { .. }) => unreachable!(),
            (Type::Placeholder { name }, t) => {
                if let Some(prev_assignment) = generic_map.insert(name.clone(), t.name()) {
                    if prev_assignment != t.name() {
                        compiler_error(
                            token,
                            "Type Error - Failed Type Resolution", 
                            vec![
                                format!("Type `{maybe_generic_t}` cannot be assigned to {concrete_t} as it was previously assigned to {prev_assignment}")
                                .as_str()
                            ]
                        );
                    }
                }
                t.name()
            }
            (Type::GenericStructBase { .. }, _) => {
                unreachable!("Base should never be on the left hand side.")
            }
            (
                Type::GenericStructInstance {
                    base: instance_base,
                    members,
                    visibility: _,
                    idents: _,
                    alias_list,
                    base_generics,
                },
                Type::ResolvedStruct {
                    name: _,
                    members: resolved_members,
                    visibility: _,
                    idents: _,
                    base: resolved_base,
                },
            ) => {
                if instance_base != resolved_base {
                    compiler_error(
                        token,
                        format!("Cannot derive {:?} from {:?}", maybe_generic_t, concrete_t)
                            .as_str(),
                        vec![],
                    );
                }

                let alias_map: HashMap<String, String> = HashMap::from_iter(
                    base_generics
                        .iter()
                        .map(|t| t.clone())
                        .zip(alias_list.iter().map(|t| t.clone())),
                );

                members
                    .iter()
                    .zip(resolved_members.iter())
                    .for_each(|(m, r)| {
                        Type::resolve_type(token, m, r, generic_map, type_map);
                    });
                alias_map.iter().for_each(|(k, alias)| {
                    let t = generic_map.get(k).unwrap().clone();
                    generic_map.remove(k);
                    Type::resolve_type(token, alias, &t, generic_map, type_map);
                });
                concrete_t.clone()
            }
            (Type::GenericStructInstance { .. }, _) => compiler_error(
                token,
                format!(
                    "Cannot resolve type {:?} into {:?}",
                    maybe_generic_t, concrete_t
                )
                .as_str(),
                vec![],
            ),
            (Type::Struct { .. }, Type::Struct { .. }) => {
                if maybe_generic_t != concrete_t {
                    compiler_error(
                        token,
                        format!(
                            "Cannot resolve type {:?} into {:?}",
                            maybe_generic_t, concrete_t
                        )
                        .as_str(),
                        vec![],
                    );
                }

                maybe_generic_t.clone()
            }
            (Type::Struct { .. }, _) => compiler_error(
                token,
                format!(
                    "Cannot resolve type {:?} into {:?}",
                    maybe_generic_t, concrete_t
                )
                .as_str(),
                vec![],
            ),
            (Type::ResolvedStruct { .. }, Type::ResolvedStruct { .. }) => {
                if maybe_generic_t != concrete_t {
                    compiler_error(
                        token,
                        format!(
                            "Cannot resolve type {:?} into {:?}",
                            maybe_generic_t, concrete_t
                        )
                        .as_str(),
                        vec![],
                    )
                }

                maybe_generic_t.clone()
            }
            (Type::ResolvedStruct { .. }, _) => compiler_error(
                token,
                format!(
                    "Cannot resolve type {:?} into {:?}",
                    maybe_generic_t, concrete_t
                )
                .as_str(),
                vec![],
            ),
            (Type::Union { .. }, _) => {
                unimplemented!("{}: Resolving unions isn't implemented yet.", token.loc)
            }
            (
                Type::GenericUnionInstance {
                    base: instance_base,
                    members,
                    alias_list,
                    base_generics,
                    ..
                },
                Type::ResolvedUnion {
                    members: resolved_members,
                    base: resolved_base,
                    ..
                },
            ) => {
                if instance_base != resolved_base {
                    compiler_error(
                        token,
                        format!("Cannot derive {:?} from {:?}", maybe_generic_t, concrete_t)
                            .as_str(),
                        vec![],
                    );
                }

                let alias_map: HashMap<String, String> = HashMap::from_iter(
                    base_generics
                        .iter()
                        .map(|t| t.clone())
                        .zip(alias_list.iter().map(|t| t.clone())),
                );

                members
                    .iter()
                    .zip(resolved_members.iter())
                    .for_each(|(m, r)| {
                        Type::resolve_type(token, m, r, generic_map, type_map);
                    });

                alias_map.iter().for_each(|(k, alias)| {
                    let t = generic_map.get(k).unwrap().clone();
                    generic_map.remove(k);
                    Type::resolve_type(token, alias, &t, generic_map, type_map);
                });

                concrete_t.clone()
            }
            (Type::GenericUnionInstance { .. }, _) => compiler_error(
                token,
                format!(
                    "Cannot resolve type {:?} into {:?}",
                    maybe_generic_t, concrete_t
                )
                .as_str(),
                vec![],
            ),
            (Type::ResolvedUnion { .. }, _) => {
                unimplemented!(
                    "{}: Resolving Resolved Union Instance isn't implemented yet.",
                    token.loc
                )
            }
            (Type::GenericUnionBase { .. }, _) => {
                panic!("GenericUnionBase should never be on the left hand side!")
            }
            (Type::Enum { .. }, Type::Enum { .. }) => {
                if maybe_generic_t != concrete_t {
                    compiler_error(
                        token,
                        format!(
                            "Cannot resolve type {:?} into {:?}",
                            maybe_generic_t, concrete_t
                        )
                        .as_str(),
                        vec![],
                    )
                }

                maybe_generic_t.clone()
            }
            (Type::Enum { .. }, _) => compiler_error(
                token,
                format!(
                    "Cannot resolve type {:?} into {:?}",
                    maybe_generic_t, concrete_t
                )
                .as_str(),
                vec![],
            ),
        }
    }

    pub fn assign_generics(
        token: &Token,
        typ: &TypeName,
        generic_map: &HashMap<TypeName, TypeName>,
        type_map: &mut HashMap<TypeName, Type>,
    ) -> TypeName {
        match type_map.get(typ).unwrap().clone() {
            Type::Placeholder { name } => generic_map
                .get(&name)
                .expect(format!("{}: Unrecognized generic: {name}", token.loc).as_str())
                .clone(),
            Type::GenericUnionInstance {
                base,
                members,
                visibility,
                idents,
                alias_list,
                base_generics,
            }
            | Type::GenericStructInstance {
                base,
                members,
                visibility,
                idents,
                alias_list,
                base_generics,
            } => {
                let alias_map: HashMap<String, String> = HashMap::from_iter(
                    base_generics
                        .iter()
                        .map(|t| t.clone())
                        .zip(alias_list.iter().map(|t| t.clone())),
                );

                // Apply the alias to the input generic map
                let aliased_generics: HashMap<String, String> = alias_map
                    .iter()
                    .map(|(k, v)| {
                        (
                            k.clone(),
                            Type::assign_generics(token, v, generic_map, type_map),
                        )
                    })
                    .collect();
                // Use the aliased generics to resolve inner types
                let resolved_members = members
                    .iter()
                    .map(|t| {
                        let _ = 0;
                        Type::assign_generics(token, t, &aliased_generics, type_map)
                    })
                    .collect::<Vec<TypeName>>();
                let mut name = base.clone();
                name.push('<');

                for (i, typ) in base_generics
                    .iter()
                    .map(|t| {
                        Type::assign_generics(
                            token,
                            alias_map.get(t).unwrap_or(t),
                            generic_map,
                            type_map,
                        )
                    })
                    .enumerate()
                {
                    if i == 0 {
                        name.push_str(typ.as_str());
                    } else {
                        name.push(' ');
                        name.push_str(typ.as_str());
                    }
                }
                name.push('>');

                let t = if matches!(
                    type_map.get(typ).unwrap(),
                    Type::GenericStructInstance { .. }
                ) {
                    Type::ResolvedStruct {
                        name,
                        members: resolved_members,
                        visibility: visibility.clone(),
                        idents: idents.clone(),
                        base: base.clone(),
                    }
                } else if matches!(
                    type_map.get(typ).unwrap(),
                    Type::GenericUnionInstance { .. }
                ) {
                    Type::ResolvedUnion {
                        name,
                        members: resolved_members,
                        visibility: visibility.clone(),
                        idents: idents.clone(),
                        base: base.clone(),
                    }
                } else {
                    unreachable!()
                };

                type_map.insert(t.name(), t.clone());
                t.name()
            }
            Type::GenericUnionBase {
                name: base,
                members,
                visibility,
                idents,
                generics,
            }
            | Type::GenericStructBase {
                name: base,
                members,
                visibility,
                idents,
                generics,
            } => {
                let resolved_members = members
                    .iter()
                    .map(|t| Type::assign_generics(token, t, generic_map, type_map))
                    .collect::<Vec<TypeName>>();
                let mut name = base.clone();
                name.push('<');

                for (i, typ) in generics
                    .iter()
                    .map(|t| generic_map.get(t).unwrap())
                    .enumerate()
                {
                    if i == 0 {
                        name.push_str(typ.as_str());
                    } else {
                        name.push(' ');
                        name.push_str(typ.as_str());
                    }
                }
                name.push('>');

                let t = if matches!(type_map.get(typ).unwrap(), Type::GenericStructBase { .. }) {
                    Type::ResolvedStruct {
                        name,
                        members: resolved_members,
                        visibility: visibility.clone(),
                        idents: idents.clone(),
                        base: base.clone(),
                    }
                } else {
                    Type::ResolvedUnion {
                        name,
                        members: resolved_members,
                        visibility: visibility.clone(),
                        idents: idents.clone(),
                        base: base.clone(),
                    }
                };

                type_map.insert(t.name(), t.clone());
                t.name()
            }
            Type::Pointer { typ } => {
                let inner = Type::assign_generics(token, &typ, generic_map, type_map);
                let t = Type::Pointer { typ: inner };
                type_map.insert(t.name(), t.clone());
                t.name()
            }
            Type::U64
            | Type::U8
            | Type::Bool
            | Type::Enum { .. }
            | Type::Struct { .. }
            | Type::ResolvedStruct { .. }
            | Type::Union { .. }
            | Type::ResolvedUnion { .. } => typ.clone(),
        }
    }

    pub fn assign_parsed_annotations(
        start_tok: &Token,
        typ_tok: &Token,
        typ: &TypeName,
        annotations: &Option<Vec<TypeName>>,
        type_map: &mut HashMap<TypeName, Type>,
    ) -> TypeName {
        match (
            type_map
                .get(typ)
                .expect(format!("Type {typ} is unknown.").as_str())
                .clone(),
            annotations,
        ) {
            (
                Type::GenericUnionBase {
                    name,
                    members,
                    visibility,
                    idents,
                    generics,
                },
                Some(annotations),
            ) => {
                if annotations.len() != generics.len() {
                    compiler_error(
                        &typ_tok,
                        "Incorrect number of generic annotations provided",
                        vec![
                            format!("Generic Union {name} is geneic over {:?}", generics).as_str(),
                            format!("Found annotations: {:?}", annotations).as_str(),
                        ],
                    )
                }
                if annotations
                    .iter()
                    .any(|t| matches!(type_map.get(t).unwrap(), Type::Placeholder { .. }))
                {
                    let t = Type::GenericUnionInstance {
                        base: name.clone(),
                        members: members.clone(),
                        visibility: visibility.clone(),
                        idents: idents.clone(),
                        alias_list: annotations.to_vec(),
                        base_generics: generics.clone(),
                    };
                    type_map.insert(t.name(), t.clone());
                    t.name()
                } else {
                    let generic_map: HashMap<TypeName, TypeName> = HashMap::from_iter(
                        generics
                            .iter()
                            .map(|t| t.clone())
                            .zip(annotations.to_vec().drain(..)),
                    );
                    Type::assign_generics(start_tok, &name, &generic_map, type_map)
                }
            }
            (Type::GenericUnionBase { name, generics, .. }, None) => compiler_error(
                &start_tok,
                format!("Casting to generic union `{name}` requires type annotations.").as_str(),
                vec![format!("Annotations are required for: {:?}", generics).as_str()],
            ),
            (
                Type::GenericStructBase {
                    name,
                    members,
                    visibility,
                    idents,
                    generics,
                },
                Some(annotations),
            ) => {
                if annotations.len() != generics.len() {
                    compiler_error(
                        &typ_tok,
                        "Incorrect number of generic annotations provided",
                        vec![
                            format!("Generic Union {name} is geneic over {:?}", generics).as_str(),
                            format!("Found annotations: {:?}", annotations).as_str(),
                        ],
                    )
                }
                if annotations
                    .iter()
                    .any(|t| matches!(type_map.get(t).unwrap(), Type::Placeholder { .. }))
                {
                    let t = Type::GenericStructInstance {
                        base: name.clone(),
                        members: members.clone(),
                        visibility: visibility.clone(),
                        idents: idents.clone(),
                        alias_list: annotations.to_vec(),
                        base_generics: generics.clone(),
                    };
                    type_map.insert(t.name(), t.clone());
                    t.name()
                } else {
                    let generic_map: HashMap<TypeName, TypeName> = HashMap::from_iter(
                        generics
                            .iter()
                            .map(|t| t.clone())
                            .zip(annotations.to_vec().drain(..)),
                    );
                    Type::assign_generics(start_tok, &name, &generic_map, type_map)
                }
            }
            (Type::Pointer { typ: inner }, Some(_)) => Type::Pointer {
                typ: Type::assign_parsed_annotations(
                    start_tok,
                    typ_tok,
                    &inner,
                    annotations,
                    type_map,
                ),
            }
            .name(),
            _ => typ.clone(),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::U64 => write!(f, "u64"),
            Type::U8 => write!(f, "u8"),
            Type::Bool => write!(f, "bool"),
            Type::Pointer { typ, .. } => write!(f, "*{typ}"),
            Type::Placeholder { name }
            | Type::Enum { name, .. }
            | Type::Struct { name, .. }
            | Type::ResolvedStruct { name, .. }
            | Type::ResolvedUnion { name, .. }
            | Type::Union { name, .. } => write!(f, "{name}"),
            Type::GenericStructBase { name, generics, .. }
            | Type::GenericUnionBase { name, generics, .. } => {
                write!(f, "{name}<{}", generics[0])?;
                for t in generics[1..].iter() {
                    write!(f, " {}", t)?;
                }
                write!(f, ">")
            }
            Type::GenericStructInstance {
                base, alias_list, ..
            }
            | Type::GenericUnionInstance {
                base, alias_list, ..
            } => {
                write!(f, "{base}<{}", alias_list[0])?;
                for t in alias_list[1..].iter() {
                    write!(f, " {}", t)?;
                }
                write!(f, ">")
            }
        }
    }
}

#[derive(Default, Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct Signature {
    pub inputs: Stack,
    pub outputs: Stack,
}
