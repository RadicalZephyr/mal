use std::{
    borrow::Cow,
    fmt,
    hash::{BuildHasher, Hash},
};

use archery::SharedPointerKind;

use once_cell::unsync::Lazy;

use regex::{Captures, Regex};

use crate::{Atom, Bool, Comment, Float, Form, Integer, List, Map, Vector};

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

impl Printable for Comment {
    fn pr(&self, options: &Options, f: &mut fmt::Formatter) -> fmt::Result {
        use crate::Comment::*;

        match self {
            Line(s) => f.write_str(s),
            Form(form) => {
                f.write_str("#_")?;
                form.pr(options, f)
            }
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
            Form::Comment(comment) => {
                if options.print_comments {
                    comment.pr(options, f)
                } else {
                    Ok(())
                }
            }
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

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Options {
    pub print_comments: bool,
    pub print_readably: bool,
}

#[derive(Copy, Clone, Debug, PartialEq)]
struct PrintForm<'a> {
    form: &'a Form,
    print_comments: bool,
    print_readably: bool,
}

impl<'a> PrintForm<'a> {
    fn new(form: &'a Form, print_comments: bool, print_readably: bool) -> PrintForm<'a> {
        PrintForm {
            form,
            print_comments,
            print_readably,
        }
    }

    fn form(&self) -> &'a Form {
        self.form
    }

    fn options(&self) -> Options {
        let PrintForm {
            print_comments,
            print_readably,
            ..
        } = *self;
        Options {
            print_comments,
            print_readably,
        }
    }

    fn as_parts(&self) -> (&'a Form, Options) {
        (self.form(), self.options())
    }
}

impl<'a> fmt::Display for PrintForm<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (form, options) = self.as_parts();
        form.pr(&options, f)
    }
}

pub fn pr_str(form: &Form, print_readably: bool) -> String {
    let print_comments = false;
    format!("{}", PrintForm::new(form, print_comments, print_readably))
}
