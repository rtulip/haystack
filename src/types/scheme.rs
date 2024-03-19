use std::collections::HashSet;

use super::{Type, TypeVar};

#[derive(Debug)]
pub struct Scheme {
    pub unbound: HashSet<TypeVar>,
    pub stack: Vec<Type>,
}
