use crate::compiler::compiler_error;
use crate::ir::{token::Token, Stack};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    U64,
    U8,
    Bool,
    Enum {
        name: String,
        variants: Vec<String>,
    },
    Placeholder {
        name: String,
    },
    Pointer {
        typ: Box<Type>,
    },
    Struct {
        name: String,
        members: Vec<Type>,
        idents: Vec<String>,
    },
    GenericStructBase {
        name: String,
        members: Vec<Type>,
        idents: Vec<String>,
        generics: Vec<Type>,
    },
    GenericStructInstance {
        base: String,
        members: Vec<Type>,
        idents: Vec<String>,
        alias_list: Vec<Type>,
        base_generics: Vec<Type>,
    },
    ResolvedStruct {
        name: String,
        members: Vec<Type>,
        idents: Vec<String>,
        base: String,
    },
    Union {
        name: String,
        members: Vec<Type>,
        idents: Vec<String>,
    },
    GenericUnionBase {
        name: String,
        members: Vec<Type>,
        idents: Vec<String>,
        generics: Vec<Type>,
    },
    GenericUnionInstance {
        base: String,
        members: Vec<Type>,
        idents: Vec<String>,
        alias_list: Vec<Type>,
        base_generics: Vec<Type>,
    },
    ResolvedUnion {
        name: String,
        members: Vec<Type>,
        idents: Vec<String>,
        base: String,
    },
}

impl Type {
    pub fn name(&self) -> String {
        match self {
            Type::GenericStructBase { name, .. } => name.clone(),
            _ => format!("{:?}", self),
        }
    }

    pub fn is_generic(&self) -> bool {
        match self {
            Type::Placeholder { .. }
            | Type::GenericStructBase { .. }
            | Type::GenericStructInstance { .. }
            | Type::GenericUnionBase { .. }
            | Type::GenericUnionInstance { .. } => true,
            Type::Pointer { typ } => typ.is_generic(),
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

    pub fn deep_check_generics(&self) -> Vec<Type> {
        match self {
            Type::U64 | Type::U8 | Type::Bool | Type::Enum { .. } => vec![],
            Type::Placeholder { .. } => vec![self.clone()],
            Type::Pointer { typ } => typ.deep_check_generics(),
            Type::Struct { members, .. }
            | Type::Union { members, .. }
            | Type::GenericStructBase { members, .. }
            | Type::GenericStructInstance { members, .. }
            | Type::ResolvedStruct { members, .. }
            | Type::GenericUnionBase { members, .. }
            | Type::GenericUnionInstance { members, .. }
            | Type::ResolvedUnion { members, .. } => {
                let mut generics = vec![];
                members.iter().for_each(|t| {
                    generics.append(&mut t.deep_check_generics());
                });
                generics
            }
        }
    }

    pub fn shallow_check_generics(&self) -> Vec<Type> {
        match self {
            Type::U64
            | Type::U8
            | Type::Bool
            | Type::Enum { .. }
            | Type::Struct { .. }
            | Type::Union { .. }
            | Type::ResolvedStruct { .. }
            | Type::ResolvedUnion { .. } => vec![],
            Type::Placeholder { .. } => vec![self.clone()],
            Type::Pointer { typ } => typ.shallow_check_generics(),
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

    pub fn size(&self) -> usize {
        match self {
            Type::U64 | Type::U8 | Type::Bool | Type::Pointer { .. } | Type::Enum { .. } => 1,
            Type::Struct { members, .. } | Type::ResolvedStruct { members, .. } => {
                members.iter().map(|t| t.size()).sum()
            }
            Type::Union { members, .. } | Type::ResolvedUnion { members, .. } => {
                members.iter().map(|t| t.size()).max().unwrap()
            }
            Type::GenericUnionBase { .. } => {
                panic!("Size of generic union base is unknown: {:?}", self)
            }
            Type::GenericUnionInstance { .. } => {
                panic!("Size of generic union instance is unknown: {:?}", self)
            }
            Type::Placeholder { .. } => panic!("Size of Placeholder types are unknown: {:?}", self),
            Type::GenericStructBase { .. } => panic!("Size of a generic struct is unknown"),
            Type::GenericStructInstance { .. } => {
                panic!("Size of a generic struct is unknown: {:?}", self)
            }
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
                Type::U64,
                Type::Pointer {
                    typ: Box::new(Type::U8),
                },
            ],
            idents: vec![String::from("size"), String::from("data")],
        }
    }

    pub fn arr_base() -> Self {
        Type::GenericStructBase {
            name: String::from("Arr"),
            members: vec![
                Type::U64,
                Type::Pointer {
                    typ: Box::new(Type::Placeholder {
                        name: String::from("T"),
                    }),
                },
            ],
            idents: vec![String::from("size"), String::from("data")],
            generics: vec![Type::Placeholder {
                name: String::from("T"),
            }],
        }
    }

    pub fn resolve_struct(token: &Token, gen_struct_t: &Type, stack: &Stack) -> Type {
        let (pairs, base, idents, generics) = match gen_struct_t {
            Type::GenericStructBase {
                name,
                members,
                idents,
                generics,
            } => {
                let pairs: Vec<(&Type, &Type)> = members
                    .iter()
                    .zip(stack[stack.len() - members.len()..].iter())
                    .collect();
                (pairs, name, idents.clone(), generics.clone())
            }
            _ => panic!("Resolve struct should only be run on Base Generic Structs...\n{}: Type: {:?} Stack: {:?}", token.loc, gen_struct_t, stack),
        };

        let mut map: HashMap<String, Type> = HashMap::new();
        let resolved_members = pairs
            .iter()
            .map(|(t1, t2)| Type::resolve_type(token, t1, t2, &mut map, &HashMap::new()))
            .collect::<Vec<Type>>();

        if !generics
            .iter()
            .all(|t| map.contains_key(&format!("{:?}", t)))
        {
            compiler_error(
                token,
                "Some types were not resolved during cast",
                vec![format!(
                    "These types were not resolved: {:?}",
                    generics
                        .iter()
                        .filter(|t| !map.contains_key(&format!("{:?}", t)))
                        .collect::<Vec<&Type>>()
                )
                .as_str()],
            )
        }

        let mut name = base.clone();
        name.push_str(format!("<{:?}", map.get(&format!("{:?}", generics[0])).unwrap()).as_str());
        for t in generics[1..].iter() {
            name.push_str(format!(" {:?}", map.get(&format!("{:?}", t)).unwrap()).as_str());
        }
        name.push('>');

        Type::ResolvedStruct {
            name,
            members: resolved_members,
            idents,
            base: base.clone(),
        }
    }

    pub fn resolve_type(
        token: &Token,
        maybe_generic_t: &Type,
        concrete_t: &Type,
        generic_map: &mut HashMap<String, Type>,
        alias_map: &HashMap<String, String>,
    ) -> Type {
        let t = match (maybe_generic_t, concrete_t) {
            (Type::U64, Type::U64) => Type::U64,
            (Type::U64, _) => compiler_error(
                token,
                format!(
                    "Cannot resolve type {:?} into {:?}",
                    maybe_generic_t, concrete_t
                )
                .as_str(),
                vec![],
            ),
            (Type::U8, Type::U8) => Type::U8,
            (Type::U8, _) => compiler_error(
                token,
                format!(
                    "Cannot resolve type {:?} into {:?}",
                    maybe_generic_t, concrete_t
                )
                .as_str(),
                vec![],
            ),
            (Type::Bool, Type::Bool) => Type::Bool,
            (Type::Bool, _) => compiler_error(
                token,
                format!(
                    "Cannot resolve type {:?} into {:?}",
                    maybe_generic_t, concrete_t
                )
                .as_str(),
                vec![],
            ),
            (Type::Pointer { typ, .. }, Type::Pointer { typ: typ2, .. }) => Type::Pointer {
                typ: Box::new(Type::resolve_type(
                    token,
                    &*typ,
                    &*typ2,
                    generic_map,
                    alias_map,
                )),
            },
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
                let name = alias_map.get(name).unwrap_or(name);
                if let Some(prev_assignment) = generic_map.insert(name.clone(), t.clone()) {
                    if prev_assignment != *t {
                        compiler_error(
                            token,
                            "Type Error - Failed Type Resolution", 
                            vec![
                                format!("Type `{:?}` cannot be assigned to {:?} as it was previously assigned to {:?}", 
                                    maybe_generic_t,
                                    concrete_t,
                                    prev_assignment
                                )
                                .as_str()
                            ]
                        );
                    }
                }

                t.clone()
            }
            (Type::GenericStructBase { .. }, _) => {
                unreachable!("Base should never be on the left hand side.")
            }
            (
                Type::GenericStructInstance {
                    base: instance_base,
                    members,
                    idents: _,
                    alias_list,
                    base_generics,
                },
                Type::ResolvedStruct {
                    name: _,
                    members: resolved_members,
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
                        .map(|t| format!("{:?}", t))
                        .zip(alias_list.iter().map(|t| format!("{:?}", t))),
                );

                members
                    .iter()
                    .zip(resolved_members.iter())
                    .for_each(|(m, r)| {
                        Type::resolve_type(token, m, r, generic_map, &alias_map);
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
                        .map(|t| format!("{:?}", t))
                        .zip(alias_list.iter().map(|t| format!("{:?}", t))),
                );

                members
                    .iter()
                    .zip(resolved_members.iter())
                    .for_each(|(m, r)| {
                        Type::resolve_type(token, m, r, generic_map, &alias_map);
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
        };

        t
    }

    pub fn assign_generics(token: &Token, typ: &Type, generic_map: &HashMap<String, Type>) -> Type {
        match typ {
            Type::Placeholder { name } => generic_map.get(name).unwrap().clone(),
            Type::GenericUnionBase { .. } => todo!("{}", token),
            Type::GenericUnionInstance {
                base,
                members,
                idents,
                alias_list,
                base_generics,
            }
            | Type::GenericStructInstance {
                base,
                members,
                idents,
                alias_list,
                base_generics,
            } => {
                let alias_map: HashMap<String, String> = HashMap::from_iter(
                    base_generics
                        .iter()
                        .map(|t| format!("{:?}", t))
                        .zip(alias_list.iter().map(|t| format!("{:?}", t))),
                );

                let resolved_members = members
                    .iter()
                    .map(|t| match t {
                        Type::Placeholder { name } => generic_map
                            .get(alias_map.get(name).unwrap())
                            .unwrap()
                            .clone(),
                        _ => Type::assign_generics(token, t, generic_map),
                    })
                    .collect::<Vec<Type>>();

                let mut name = base.clone();
                name.push('<');

                for (i, typ) in base_generics
                    .iter()
                    .map(|t| {
                        let alias = alias_map.get(&format!("{:?}", t)).unwrap();
                        generic_map.get(alias).unwrap()
                    })
                    .enumerate()
                {
                    if i == 0 {
                        name.push_str(format!("{:?}", typ).as_str());
                    } else {
                        name.push(' ');
                        name.push_str(format!("{:?}", typ).as_str());
                    }
                }
                name.push('>');

                if matches!(typ, Type::GenericStructInstance { .. }) {
                    Type::ResolvedStruct {
                        name,
                        members: resolved_members,
                        idents: idents.clone(),
                        base: base.clone(),
                    }
                } else if matches!(typ, Type::GenericUnionInstance { .. }) {
                    Type::ResolvedUnion {
                        name,
                        members: resolved_members,
                        idents: idents.clone(),
                        base: base.clone(),
                    }
                } else {
                    unreachable!()
                }
            }
            Type::GenericStructBase {
                name: base,
                members,
                idents,
                generics,
            } => {
                let resolved_members = members
                    .iter()
                    .map(|t| Type::assign_generics(token, t, generic_map))
                    .collect::<Vec<Type>>();
                let mut name = base.clone();
                name.push('<');

                for (i, typ) in generics
                    .iter()
                    .map(|t| generic_map.get(&t.name()).unwrap())
                    .enumerate()
                {
                    if i == 0 {
                        name.push_str(format!("{:?}", typ).as_str());
                    } else {
                        name.push(' ');
                        name.push_str(format!("{:?}", typ).as_str());
                    }
                }
                name.push('>');

                Type::ResolvedStruct {
                    name,
                    members: resolved_members,
                    idents: idents.clone(),
                    base: base.clone(),
                }
            }
            Type::Pointer { typ } => Type::Pointer {
                typ: Box::new(Type::assign_generics(token, &*typ, generic_map)),
            },
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
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::U64 => write!(f, "u64"),
            Type::U8 => write!(f, "u8"),
            Type::Bool => write!(f, "bool"),
            Type::Pointer { typ, .. } => write!(f, "*{:?}", *typ),
            Type::Placeholder { name }
            | Type::Enum { name, .. }
            | Type::Struct { name, .. }
            | Type::ResolvedStruct { name, .. }
            | Type::ResolvedUnion { name, .. }
            | Type::Union { name, .. } => write!(f, "{name}"),
            Type::GenericStructBase { name, generics, .. }
            | Type::GenericUnionBase { name, generics, .. } => {
                write!(f, "{name}<{:?}", generics[0])?;
                for t in generics[1..].iter() {
                    write!(f, " {:?}", t)?;
                }
                write!(f, ">")
            }
            Type::GenericStructInstance {
                base, alias_list, ..
            }
            | Type::GenericUnionInstance {
                base, alias_list, ..
            } => {
                write!(f, "{base}<{:?}", alias_list[0])?;
                for t in alias_list[1..].iter() {
                    write!(f, " {:?}", t)?;
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
