use std::hash::{Hash, Hasher};

use derive_more::{Deref, Display, From, FromStr};

use rug::{
    Complex as RugComplex, Float as RugFloat, Integer as RugInteger, Rational as RugRational,
};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Atom {
    Bool(Bool),
    Complex(Complex),
    Float(Float),
    Integer(Integer),
    Keyword(String),
    Nil,
    Rational(Rational),
    String(String),
    Symbol(String),
}

#[derive(Clone, Debug, Display, Hash, PartialEq, Eq)]
pub enum Bool {
    True,
    False,
}

#[derive(Clone, Debug, Deref, Display, From)]
pub struct Complex(pub RugComplex);

impl Complex {
    fn limited_repr(&self) -> String {
        format!("{:.31}", self.0)
    }
}

impl Hash for Complex {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.limited_repr().hash(state)
    }
}

impl PartialEq for Complex {
    fn eq(&self, other: &Self) -> bool {
        self.limited_repr().eq(&other.limited_repr())
    }
}

impl Eq for Complex {}

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

#[derive(Clone, Debug, Deref, Display, From, FromStr, Hash, PartialEq, Eq)]
pub struct Rational(pub RugRational);
