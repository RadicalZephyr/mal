use std::hash::{Hash, Hasher};

use derive_more::{Deref, Display, From, FromStr};

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
    Vector(Vector<Form>),
}
