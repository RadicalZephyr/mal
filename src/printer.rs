use std::fmt::Write;

use crate::Form;

fn pr_str_iter(form: &Form, buffer: &mut String) {
    match form {
        Form::Atom(atom) => write!(buffer, "{:?}", atom).unwrap(),
        Form::List(forms) => {
            buffer.write_str("(").unwrap();
            for form in forms {
                pr_str_iter(form, buffer)
            }
            buffer.write_str(")").unwrap();
        }
    }
}

pub fn pr_str(form: &Form) -> String {
    let mut buffer = String::new();
    pr_str_iter(form, &mut buffer);
    buffer
}
