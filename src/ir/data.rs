use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum InitData {
    String(String),
    Arr { size: u64, pointer: String },
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum UninitData {
    Marker,
    Region(u64),
}
