use rug::{Float, Integer};

#[derive(Clone, Debug, PartialEq)]
pub enum Atom {
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
}
