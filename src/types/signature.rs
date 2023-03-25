use crate::error::HayError;
use crate::lex::token::Token;
use std::collections::HashMap;

use super::{stack_compare, TypeId, TypeMap, Variance};
type Predicate = dyn Fn(&Vec<TypeId>, &TypeMap) -> bool;

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
            panic!("{self:?} already had a predicate.");
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
        types: &mut TypeMap,
        variance: Variance,
    ) -> Result<Option<HashMap<TypeId, TypeId>>, HayError> {
        // Make sure the stack has at least as many elements as the inputs.
        // This ensures that the stack doesn't underflow.
        if stack.len() < self.inputs.len() {
            return Err(HayError::new_type_err(
                format!("Invalid number of inputs for {:?}", token.lexeme),
                token.loc.clone(),
            )
            .with_hint(format!("Expected: {:?}", self.inputs))
            .with_hint(format!("Found:    {stack:?}")));
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
        if !stack_compare(&sig.inputs, stack, types, variance) {
            return Err(HayError::new_type_err(
                format!("Invalid inputs for `{}`", token.lexeme).as_str(),
                token.loc.clone(),
            )
            .optional_hint(token.fn_name(types, &vec![]), |name| {
                format!("Function `{}` was defined here: {}", name.lexeme, name.loc)
            })
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

        // If the signature has a predicate, evaulate it.
        if let Some((pred, msg)) = &sig.predicate {
            if !pred(&sig.inputs, types) {
                return Err(HayError::new_type_err(
                    format!("Invalid inputs for {:?}", token.lexeme).as_str(),
                    token.loc.clone(),
                )
                .optional_hint(token.fn_name(types, &vec![]), |name| {
                    format!("Function `{}` was defined here: {}", name.lexeme, name.loc)
                })
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
        types: &mut TypeMap,
        variance: Variance,
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
                e = e.with_hint(format!("{sig:?}",));
            }
            return Err(e);
        }

        // Evaluate each Signature & return early if successful.
        for sig in sigs {
            if let Ok(x) = sig.evaluate(token, stack, types, variance) {
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
        types: &mut TypeMap,
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
        types: &mut TypeMap,
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
