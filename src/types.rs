use num_bigint::BigInt;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Atom {
    Integer(BigInt),
    Keyword(String),
    Nil,
    String(String),
    Symbol(String),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Form {
    Atom(Atom),
    List(Vec<Form>),
}
