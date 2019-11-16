use std::{
    collections::hash_map::RandomState,
    hash::{BuildHasher, Hash, Hasher},
    iter::{FromIterator, IntoIterator},
};

use archery::{RcK, SharedPointerKind};

use derive_more::{Deref, Display, From};

use rpds::map::hash_trie_map::Iter as HashTrieMapIter;
use rpds::HashTrieMap;
pub use rpds::{List, Vector};

#[derive(Debug, Deref, Display, From)]
pub struct Map<K, V, P = RcK, H = RandomState>(pub HashTrieMap<K, V, P, H>)
where
    K: Hash + Eq,
    P: SharedPointerKind,
    H: BuildHasher + Clone;

impl<K, V, P, H> Map<K, V, P, H>
where
    K: Hash + Eq,
    P: SharedPointerKind,
    H: BuildHasher + Clone + Default,
{
    pub fn new() -> Self {
        Map(HashTrieMap::new_with_hasher_and_ptr_kind(H::default()))
    }

    pub fn insert(&self, key: K, value: V) -> Self {
        Map(self.0.insert(key, value))
    }
}

impl<K, V, P, H> Clone for Map<K, V, P, H>
where
    K: Eq + Hash,
    P: SharedPointerKind,
    H: BuildHasher + Clone,
{
    fn clone(&self) -> Self {
        Map(self.0.clone())
    }
}

impl<K, V, P, H> FromIterator<(K, V)> for Map<K, V, P, H>
where
    K: Eq + Hash,
    P: SharedPointerKind,
    H: BuildHasher + Clone + Default,
{
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = (K, V)>,
    {
        Map(iter.into_iter().collect())
    }
}

impl<K, V, P, H> Hash for Map<K, V, P, H>
where
    K: Hash + Eq,
    V: Hash,
    P: SharedPointerKind,
    H: BuildHasher + Clone,
{
    fn hash<H2: Hasher>(&self, state: &mut H2) {
        for (k, v) in self.0.iter() {
            k.hash(state);
            v.hash(state);
        }
    }
}

impl<'a, K, V, P, H> IntoIterator for &'a Map<K, V, P, H>
where
    K: Hash + Eq,
    P: SharedPointerKind,
    H: BuildHasher + Clone + Default,
{
    type Item = (&'a K, &'a V);
    type IntoIter = HashTrieMapIter<'a, K, V, P>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<K, V, P, H> PartialEq for Map<K, V, P, H>
where
    K: Hash + Eq,
    V: PartialEq,
    P: SharedPointerKind,
    H: BuildHasher + Clone,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}

impl<K, V, P, H> Eq for Map<K, V, P, H>
where
    K: Hash + Eq,
    V: PartialEq,
    P: SharedPointerKind,
    H: BuildHasher + Clone,
{
}
