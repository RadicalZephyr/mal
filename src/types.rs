use std::{hash::Hash, iter::IntoIterator};

pub use rpds::{List, Vector};

use rug::{Assign, Float as RugFloat, Integer as RugInteger, Rational as RugRational};

mod atoms;
pub use atoms::{Atom, Bool, Float, Integer, Rational};

mod sequences;
pub use sequences::Map;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Comment {
    Line(String),
    Form(Box<Form>),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Form {
    Atom(Atom),
    Comment(Comment),
    List(List<Form>),
    Map(Map<Form, Form>),
    Vector(Vector<Form>),
}

impl Form {
    pub fn empty_list() -> Form {
        Form::List(List::new())
    }

    pub fn list<F: IntoIterator<Item = Form>>(forms: F) -> Form {
        Form::List(forms.into_iter().collect())
    }

    pub fn empty_map() -> Form {
        Form::Map(Map::new())
    }

    pub fn map<F: IntoIterator<Item = (Form, Form)>>(forms: F) -> Form {
        Form::Map(forms.into_iter().collect())
    }

    pub fn empty_vector() -> Form {
        Form::Vector(Vector::new())
    }

    pub fn vector<F: IntoIterator<Item = Form>>(forms: F) -> Form {
        Form::Vector(forms.into_iter().collect())
    }

    pub fn nil() -> Form {
        Form::Atom(Atom::Nil)
    }

    pub fn _true() -> Form {
        Form::Atom(Atom::Bool(Bool::True))
    }

    pub fn _false() -> Form {
        Form::Atom(Atom::Bool(Bool::False))
    }

    pub fn float<T>(x: T) -> Form
    where
        RugFloat: Assign<T>,
    {
        Form::Atom(Atom::Float(Float(RugFloat::with_val(54, x))))
    }

    pub fn integer(x: impl Into<RugInteger>) -> Form {
        Form::Atom(Atom::Integer(Integer(x.into())))
    }

    pub fn rational(x: impl Into<RugRational>) -> Form {
        Form::Atom(Atom::Rational(Rational(x.into())))
    }
}
