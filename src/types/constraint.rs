use super::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constraint {
    Equal(Type, Type),
    Subtype { parent: Type, child: Type },
}

impl Constraint {
    pub fn stack_compare(mut expected: Vec<Type>, mut found: Vec<Type>) -> Vec<Self> {
        if expected.len() > found.len() {
            found.extend(std::iter::repeat(Type::Never).take(expected.len() - found.len()))
        } else if found.len() > expected.len() {
            expected.extend(std::iter::repeat(Type::Never).take(found.len() - expected.len()))
        }

        expected
            .into_iter()
            .zip(found.into_iter())
            .map(|(expected, found)| Constraint::Subtype {
                parent: expected,
                child: found,
            })
            .collect()
    }
}
