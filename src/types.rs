pub use rug::{Float, Integer};

#[derive(Clone, Debug, PartialEq)]
pub enum Bool {
    True,
    False,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Atom {
    Bool(Bool),
    Float(Float),
    Integer(Integer),
    Keyword(String),
    Nil,
    String(String),
    Symbol(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Form {
    Atom(Atom),
    List(Vec<Form>),
    Vector(Vec<Form>),
}
