#[derive(Debug)]
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
        let a = self.find_key(a);
        let b = self.find_key(b);

        if a == b {
            return Ok(());
        }

        let value = Self::unify_values(&self.parents[a.into()].1, &self.parents[b.into()].1)?;

        self.parents[a.into()].1 = value;
        self.parents[b.into()].0 = a;

        Ok(())
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

    #[test]
    fn unify_preserves_assigned_value() {
        let mut tbl = UnificationTable::<usize, bool>::new();

        let a = tbl.new_key();
        let b = tbl.new_key();

        tbl.assign_key(b, Some(true)).unwrap();
        tbl.unify_keys(a, b).unwrap();

        assert_eq!(tbl.get(a), &Some(true));
        assert_eq!(tbl.get(b), &Some(true));
    }

    #[test]
    fn unify_keys_preserves_values() {
        let mut tbl = UnificationTable::<usize, bool>::new();

        // None + Some
        let a = tbl.new_key();
        let b = tbl.new_key();
        tbl.assign_key(b, Some(true)).unwrap();
        tbl.unify_keys(a, b).unwrap();
        assert_eq!(tbl.get(a), &Some(true));
        assert_eq!(tbl.get(b), &Some(true));

        // Some + None
        let c = tbl.new_key();
        tbl.unify_keys(a, c).unwrap();
        assert_eq!(tbl.get(c), &Some(true));

        // Some + same Some
        let d = tbl.new_key();
        tbl.assign_key(d, Some(true)).unwrap();
        tbl.unify_keys(c, d).unwrap();
        assert_eq!(tbl.get(d), &Some(true));

        // Some + conflicting Some
        let e = tbl.new_key();
        tbl.assign_key(e, Some(false)).unwrap();

        assert!(tbl.unify_keys(a, e).is_err());
        assert_eq!(tbl.get(a), &Some(true));
        assert_eq!(tbl.get(e), &Some(false));

        // Failure must not merge the keys.
        tbl.assign_key(e, Some(false)).unwrap();
        assert!(tbl.assign_key(e, Some(true)).is_err());
    }
}
