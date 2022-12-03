use crate::types::TypeId;

#[derive(Debug, Clone)]
pub enum StmtKind {
    Var,
    Function,
    InterfaceFunction(TypeId),
}
