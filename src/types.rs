use std::{
    collections::hash_map::RandomState,
    hash::{BuildHasher, Hash, Hasher},
    iter::IntoIterator,
};

use archery::{RcK, SharedPointerKind};

use derive_more::{Deref, Display, From, FromStr};

use rpds::map::hash_trie_map::Iter as HashTrieMapIter;
use rpds::HashTrieMap;
pub use rpds::{List, Vector};

use rug::{Float as RugFloat, Integer as RugInteger};

#[derive(Clone, Debug, Deref, Display, From)]
pub struct Float(pub RugFloat);

impl Float {
    fn limited_repr(&self) -> String {
        format!("{:.15}", self.0)
    }
}

impl Hash for Float {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.limited_repr().hash(state)
    }
}

impl PartialEq for Float {
    fn eq(&self, other: &Self) -> bool {
        self.limited_repr().eq(&other.limited_repr())
    }
}

impl Eq for Float {}

#[derive(Clone, Debug, Deref, Display, From, FromStr, Hash, PartialEq, Eq)]
pub struct Integer(pub RugInteger);

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

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Bool {
    True,
    False,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Atom {
    Bool(Bool),
    Float(Float),
    Integer(Integer),
    Keyword(String),
    Nil,
    String(String),
    Symbol(String),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Form {
    Atom(Atom),
    List(List<Form>),
    Map(Map<Form, Form>),
    Vector(Vector<Form>),
}

impl Form {
    pub fn nil() -> Form {
        Form::Atom(Atom::Nil)
    }
}
