use crate::compiler::compiler_error;
use crate::ir::{token::Token, Stack};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    U64,
    Bool,
    Ptr,
    Placeholder {
        name: String,
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
    GenericStructInstance{
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
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::U64 | Type::Bool | Type::Ptr => 1,
            Type::Placeholder { .. } => panic!("Size of Placeholder types are unknown"),
            Type::GenericStructBase { .. } => panic!("Size of a generic struct is unknown"),
            Type::GenericStructInstance { .. } => panic!("Size of a generic struct is unknown"),
            Type::Struct {
                name: _, members, ..
            }
            | Type::ResolvedStruct {
                name: _, members, ..
            } => members.iter().map(|t| t.size()).sum(),
        }
    }

    pub fn str() -> Self {
        Type::Struct {
            name: String::from("Str"),
            members: vec![Type::U64, Type::Ptr],
            idents: vec![String::from("size"), String::from("data")],
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
            _ => todo!(),
        };

        let mut map: HashMap<String, Type> = HashMap::new();
        let resolved_members = pairs
            .iter()
            .map(|(t1, t2)| Type::resolve_type(&token, t1, t2, &mut map))
            .collect::<Vec<Type>>();

        if !generics.iter().all(|t| map.contains_key(&format!("{:?}", t))) {
            compiler_error(
                token, 
                "Some types were not resolved during cast", 
                vec![
                    format!(
                        "These types were not resolved: {:?}", 
                        generics.iter().filter(|t| !map.contains_key(&format!("{:?}", t))).collect::<Vec<&Type>>()
                    ).as_str()
                    
                ]
            )
        }


        let mut name = base.clone();
        name.push_str(format!("<{:?}", map.get(&format!("{:?}",generics[0])).unwrap()).as_str());
        for t in generics[1..].iter() {
            name.push_str(format!(" {:?}", map.get(&format!("{:?}",t)).unwrap()).as_str());
        }
        name.push('>');
        
        Type::ResolvedStruct {
            name, 
            members: resolved_members,
            idents,
            base: base.clone()
        }
    }

    pub fn resolve_type(
        token: &Token,
        maybe_generic_t: &Type,
        concrete_t: &Type,
        generic_map: &mut HashMap<String, Type>,
    ) -> Type {
        
        let t = match (maybe_generic_t, concrete_t) {
            (Type::U64, Type::U64) => Type::U64,
            (Type::U64, _) => todo!("compiler_error"),
            (Type::Bool, Type::Bool) => Type::Bool,
            (Type::Bool, _) => todo!("compiler_error"),
            (Type::Ptr, Type::Ptr) => Type::Ptr,
            (Type::Ptr, _) => todo!("compiler_error"),
            (Type::Placeholder { .. }, Type::GenericStructBase { .. }) => unreachable!(),
            (Type::Placeholder { .. }, Type::GenericStructInstance { .. }) => unreachable!(),
            (Type::Placeholder { name }, t) => {
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
            (Type::GenericStructBase { .. }, _) => unreachable!("Base should never be on the left hand side."),
            (Type::GenericStructInstance {
                base: instance_base,
                members,
                idents: _,
                alias_list,
                base_generics,
            }, Type::ResolvedStruct {
                name: _, 
                members: resolved_members,
                idents: _,
                base: resolved_base,
            }) => {
                if instance_base != resolved_base {
                    compiler_error(token, format!("Cannot derive {:?} from {:?}", maybe_generic_t, concrete_t).as_str(), vec![]);
                }

                let alias_map: HashMap<String, String> = HashMap::from_iter(base_generics.iter().map(|t| format!("{:?}", t)).zip(alias_list.iter().map(|t| format!("{:?}", t))));


                members.iter().zip(resolved_members.iter()).for_each(|(m, r)| {
                    match m {
                        Type::Placeholder { name } => {
                            
                            let alias = alias_map.get(name).unwrap();
                            if let Some(prev_assignment) = generic_map.insert(alias.clone(), r.clone()) {
                                if prev_assignment != *r {
                                    compiler_error(
                                        token, 
                                        "Type Error - Failed Type Resolution", 
                                        vec![
                                            format!("Type `{:?}` cannot be assigned to {:?} as it was previously assigned to {:?}", 
                                                m, 
                                                r, 
                                                prev_assignment
                                            )
                                            .as_str()
                                        ]
                                    );
                                }
                                
                            }
                        }
                        _ => (),
                    }
                });
                
                concrete_t.clone()
            },
            (Type::GenericStructInstance {..}, _) => todo!("compiler error"),
            (Type::Struct { .. }, Type::Struct { .. }) => {
                todo!("Check that these are the same struct")
            }
            (Type::Struct { .. }, _) => todo!("compiler_error"),
            (Type::ResolvedStruct { .. }, Type::ResolvedStruct { .. }) => {
                todo!("Check that these are the same")
            }
            (Type::ResolvedStruct { .. }, _) => todo!("compiler error"),
        };

        t
    }

    pub fn assign_generics(token: &Token, typ: &Type, generic_map: &HashMap<String, Type>) -> Type {


        match typ {
            Type::Placeholder {name} => generic_map.get(name).unwrap().clone(),
            Type::GenericStructInstance{ base, members, idents, alias_list, base_generics } => {
                
                let alias_map: HashMap<String, String> = HashMap::from_iter(
                    base_generics.iter().map(|t| format!("{:?}", t))
                    .zip(alias_list.iter().map(|t| format!("{:?}", t)))
                );

                let resolved_members = members.iter().map(|t| match t {
                    Type::Placeholder { name } => generic_map.get(alias_map.get(name).unwrap()).unwrap().clone(),
                    Type::GenericStructInstance {..} => Type::assign_generics(token, t, generic_map),
                    t => t.clone()
                }).collect::<Vec<Type>>();
                

                let mut name = base.clone();
                name.push('<');

                for (i, typ) in base_generics.iter().map(|t| {
                    let alias = alias_map.get(&format!("{:?}", t)).unwrap();
                    generic_map.get(alias).unwrap()
                }).enumerate() {
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
                    base: base.clone()
                }


            }
            Type::GenericStructBase { ..} => unreachable!(),
            t => t.clone()
        }

    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::U64 => write!(f, "u64"),
            Type::Bool => write!(f, "bool"),
            Type::Ptr => write!(f, "ptr"),
            Type::Placeholder { name } => write!(f, "{name}"),
            Type::Struct { name, .. } | Type::ResolvedStruct { name, .. } => write!(f, "{name}"),
            Type::GenericStructBase {
                name,
                members: _,
                idents: _,
                generics,
            } => {
                write!(f, "{name}<{:?}", generics[0])?;
                for t in generics[1..].iter() {
                    write!(f, " {:?}", t)?;
                }
                write!(f, ">")
            }
            Type::GenericStructInstance {
                base,
                alias_list,
                ..
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
