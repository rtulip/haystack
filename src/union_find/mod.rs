pub struct UnifiactionError<V>(pub V, pub V);

pub struct UnificationTable<K, V>
where
    K: PartialEq + From<usize> + Into<usize> + Copy,
    V: Clone + PartialEq,
{
    parents: Vec<(K, Option<V>)>,
}

impl<'a, K, V> UnificationTable<K, V>
where
    K: PartialEq + From<usize> + Into<usize> + Copy,
    V: Clone + PartialEq,
{
    pub fn new() -> Self {
        Self { parents: vec![] }
    }

    fn unify_values(left: &Option<V>, right: &Option<V>) -> Result<Option<V>, UnifiactionError<V>> {
        match (left, right) {
            (None, None) => Ok(None),
            (None, Some(v)) | (Some(v), None) => Ok(Some(v.clone())),
            (Some(left), Some(right)) => {
                if left == right {
                    Ok(Some(left.clone()))
                } else {
                    Err(UnifiactionError(left.clone(), right.clone()))
                }
            }
        }
    }

    pub fn new_key(&mut self) -> K {
        let len = self.parents.len();
        let key = K::from(len);
        self.parents.push((key, None));
        key
    }

    pub fn find_key(&self, k: K) -> K {
        let root = self.parents[k.into()].0.clone();
        if root != k {
            self.find_key(root)
        } else {
            k
        }
    }

    pub fn get(&self, k: K) -> &Option<V> {
        let root = self.find_key(k);
        &self.parents[root.into()].1
    }

    fn get_mut(&mut self, k: K) -> &mut Option<V> {
        let root = self.find_key(k);
        &mut self.parents[root.into()].1
    }

    fn unify(&mut self, a: K, b: K) {
        let b = self.find_key(b);
        self.parents[b.into()].0 = self.find_key(a);
    }

    pub fn unify_keys(&mut self, a: K, b: K) -> Result<(), UnifiactionError<V>> {
        Self::unify_values(self.get(a), self.get(b))?;
        Ok(self.unify(a, b))
    }

    pub fn assign_key(&mut self, k: K, value: Option<V>) -> Result<(), UnifiactionError<V>> {
        let value = Self::unify_values(self.get(k), &value)?;
        *self.get_mut(k) = value;

        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn assign() {
        let mut tbl = UnificationTable::<usize, bool>::new();

        let k = tbl.new_key();
        assert!(tbl.get(k).is_none());
        assert!(tbl.assign_key(k, None).is_ok());
        assert!(tbl.get(k).is_none());
        assert!(tbl.assign_key(k, Some(true)).is_ok());
        assert!(tbl.get(k) == &Some(true));
        assert!(tbl.assign_key(k, None).is_ok());
        assert!(tbl.get(k) == &Some(true));
        assert!(tbl.assign_key(k, Some(false)).is_err());
    }
}
