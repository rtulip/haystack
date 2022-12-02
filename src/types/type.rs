use crate::ast::member::TypedMember;
use crate::ast::stmt::GlobalEnv;
use crate::error::HayError;
use crate::lex::token::Token;
use std::collections::BTreeMap;

use super::{
    Function, GenericFunction, InterfaceBaseType, RecordKind, TypeId, TypeMap, UncheckedFunction,
};

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
    RecordPreDeclaration {
        token: Token,
        name: Token,
        kind: RecordKind,
        generics: Vec<TypeId>,
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
    GenericFunction {
        func: GenericFunction,
    },
    /// Represents a concrete function that needs to be type checked.
    UncheckedFunction {
        func: UncheckedFunction,
    },
    /// Represents a function that has been type checked.
    Function {
        func: Function,
    },

    InterfaceBase(InterfaceBaseType),
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
            Type::InterfaceBase(base) => base.id(),
            Type::UncheckedFunction { .. }
            | Type::GenericFunction { .. }
            | Type::Function { .. } => {
                unimplemented!("Haven't implemented name from Functions.")
            }
            Type::RecordPreDeclaration { .. } => unreachable!(),
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
                let func = func.type_check(global_env, types)?;
                types.insert(tid, Type::Function { func });
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
