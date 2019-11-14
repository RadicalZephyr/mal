use std::{
    borrow::Cow,
    fmt,
    hash::{BuildHasher, Hash},
};

use archery::SharedPointerKind;

use once_cell::unsync::Lazy;

use regex::{Captures, Regex};

use crate::{Atom, Bool, Float, Form, Integer, List, Map, Vector};

pub struct Options {
    pub print_readably: bool,
}

trait Printable {
    fn pr(&self, options: &Options, f: &mut fmt::Formatter) -> fmt::Result;
}

fn print_string<'a>(s: &'a str) -> Cow<'a, str> {
    let re: Lazy<Regex> = Lazy::new(|| {
        Regex::new(
            r#"("|\\|
)"#,
        )
        .unwrap()
    });
    re.replace_all(s, |captures: &Captures| {
        match captures.get(0).unwrap().as_str() {
            "\"" => r#"\""#,
            "\\" => r"\\",
            "\n" => r"\n",
            _ => unreachable!(),
        }
    })
}

impl Printable for Atom {
    fn pr(&self, options: &Options, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::Atom::*;

        match self {
            Bool(value) => value.pr(options, f),
            Float(value) => value.pr(options, f),
            Integer(value) => value.pr(options, f),
            Symbol(name) | Keyword(name) => f.write_str(&name),
            String(contents) => write!(f, "\"{}\"", print_string(contents)),
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

fn print_items<'a, I>(items: I, options: &Options, f: &mut fmt::Formatter) -> fmt::Result
where
    I: IntoIterator<Item = &'a Form>,
{
    let mut sep = "";
    for form in items {
        f.write_str(sep)?;
        form.pr(options, f)?;
        sep = " ";
    }
    Ok(())
}

impl Printable for List<Form> {
    fn pr(&self, options: &Options, f: &mut fmt::Formatter) -> fmt::Result {
        print_items(self, options, f)
    }
}

impl<K, V, P, H> Printable for Map<K, V, P, H>
where
    K: Hash + Eq + Printable,
    V: Printable,
    P: SharedPointerKind,
    H: BuildHasher + Clone + Default,
{
    fn pr(&self, options: &Options, f: &mut fmt::Formatter) -> fmt::Result {
        let mut sep = "";
        for (k, v) in self {
            f.write_str(sep)?;
            k.pr(options, f)?;
            f.write_str(" ")?;
            v.pr(options, f)?;
            sep = " ";
        }
        Ok(())
    }
}

impl Printable for Vector<Form> {
    fn pr(&self, options: &Options, f: &mut fmt::Formatter) -> fmt::Result {
        print_items(self, options, f)
    }
}

impl Printable for Form {
    fn pr(&self, options: &Options, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Form::Atom(atom) => atom.pr(options, f),
            Form::List(forms) => {
                f.write_str("(")?;
                forms.pr(options, f)?;
                f.write_str(")")
            }
            Form::Map(map) => {
                f.write_str("{")?;
                map.pr(options, f)?;
                f.write_str("}")
            }
            Form::Vector(forms) => {
                f.write_str("[")?;
                forms.pr(options, f)?;
                f.write_str("]")
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
