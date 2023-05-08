use crate::types::Frame;

pub struct TypedGetAddressOfFramedExpr {
    pub frame: Frame,
    pub idx: usize,
    pub inner: Option<Vec<String>>,
}
