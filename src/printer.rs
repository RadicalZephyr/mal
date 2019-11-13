use std::fmt;

use crate::{Atom, Bool, Float, Form, Integer};

pub struct Options {
    pub print_readably: bool,
}

trait Printable {
    fn pr(&self, options: &Options, f: &mut fmt::Formatter) -> fmt::Result;
}

impl Printable for Atom {
    fn pr(&self, options: &Options, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::Atom::*;

        match self {
            Bool(value) => value.pr(options, f),
            Float(value) => value.pr(options, f),
            Integer(value) => value.pr(options, f),
            Symbol(name) | Keyword(name) => f.write_str(&name),
            String(contents) => write!(f, "\"{}\"", contents),
            Nil => f.write_str("nil"),
        }
    }
}

impl Printable for Bool {
    fn pr(&self, _options: &Options, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::Bool::*;

        match self {
            True => f.write_str("true"),
            False => f.write_str("false"),
        }
    }
}

impl Printable for Integer {
    fn pr(&self, _options: &Options, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

impl Printable for Float {
    fn pr(&self, _options: &Options, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

impl Printable for Form {
    fn pr(&self, options: &Options, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Form::Atom(atom) => atom.pr(options, f),
            Form::List(forms) => {
                let mut sep = "";
                f.write_str("(")?;
                for form in forms {
                    f.write_str(sep)?;
                    form.pr(options, f)?;
                    sep = " ";
                }
                f.write_str(")")
            }
        }
    }
}

struct PrintForm<'a>(bool, &'a Form);

impl<'a> fmt::Display for PrintForm<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrintForm(print_readably, ref form) = &self;
        let options = Options {
            print_readably: *print_readably,
        };
        form.pr(&options, f)
    }
}

pub fn pr_str(form: &Form, print_readably: bool) -> String {
    format!("{}", PrintForm(print_readably, form))
}
