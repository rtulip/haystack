use crate::types::Frame;

#[derive(Debug, Clone)]
pub struct TypedGetAddressOfFramedExpr {
    pub frame: Frame,
    pub idx: usize,
    pub inner: Option<Vec<String>>,
}
