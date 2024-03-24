use super::{Type, TypeCheckError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constraint {
    Equal(Type, Type),
    Subtype { parent: Type, child: Type },
}

impl Constraint {
    pub fn stack_compare(
        mut expected: Vec<Type>,
        mut found: Vec<Type>,
    ) -> Result<Vec<Self>, TypeCheckError> {
        if expected.len() != found.len() {
            return Err(TypeCheckError::StackLengthsDiffer);
        }

        Ok(expected
            .into_iter()
            .zip(found.into_iter())
            .map(|(expected, found)| Constraint::Subtype {
                parent: expected,
                child: found,
            })
            .collect())
    }
}
