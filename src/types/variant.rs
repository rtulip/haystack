use crate::types::TypeId;

#[derive(Debug, Clone)]
pub struct VariantType {
    pub base: TypeId,
    pub variant: String,
}
