/// Representation of the different kinds of records.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum RecordKind {
    Struct,
    Union,
}

impl std::fmt::Display for RecordKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RecordKind::Struct => write!(f, "struct"),
            RecordKind::Union => write!(f, "union"),
        }
    }
}
