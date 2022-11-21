use crate::ast::member::TypedMember;
use crate::ast::stmt::GlobalEnv;
use crate::error::HayError;
use crate::lex::token::Token;
use std::collections::{BTreeMap, HashMap};

use super::{
    FramedType, Function, GenericFunction, RecordKind, TypeId, TypeMap, UncheckedFunction,
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

                if func.inputs.first().is_some() && func.inputs.first().unwrap().ident.is_some() {
                    func.inputs.iter().rev().for_each(|arg| {
                        frame.push((
                            arg.ident.as_ref().unwrap().lexeme.clone(),
                            FramedType {
                                origin: arg.token.clone(),
                                typ: arg.typ.clone(),
                                mutable: arg.mutable.is_some(),
                            },
                        ))
                    });
                } else {
                    func.inputs
                        .iter()
                        .for_each(|arg| stack.push(arg.typ.clone()));
                }

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
