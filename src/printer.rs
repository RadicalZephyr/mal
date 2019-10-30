use std::fmt;

use crate::{Atom, Form};

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::Atom::*;
        use crate::Bool::*;

        match self {
            Bool(value) => match value {
                True => f.write_str("true"),
                False => f.write_str("false"),
            },
            Float(value) => value.fmt(f),
            Integer(value) => value.fmt(f),
            Symbol(name) | Keyword(name) => f.write_str(&name),
            String(contents) => write!(f, r#""{}""#, contents),
            Nil => f.write_str("nil"),
        }
    }
}

impl fmt::Display for Form {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Form::Atom(atom) => atom.fmt(f),
            Form::List(forms) => {
                let mut sep = "";
                f.write_str("(")?;
                for form in forms {
                    write!(f, "{}{}", sep, form)?;
                    sep = " ";
                }
                f.write_str(")")
            }
        }
    }
}

pub fn pr_str(form: &Form) -> String {
    format!("{}", form)
}
