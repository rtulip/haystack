use crate::types::Frame;

pub struct TypedGetFrameExpr {
    pub frame: Frame,
    pub idx: usize,
    pub inner: Option<Vec<String>>,
}
