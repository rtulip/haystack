use super::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterfaceType {
    pub iface: String,
    pub types: Vec<Type>,
}
