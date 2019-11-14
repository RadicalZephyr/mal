use crate::Form;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Error {
    Bad,
}

pub fn read_str2(input: &str) -> Result<Form, Error> {
    match input {
        "nil" => Ok(Form::nil()),
        "true" => Ok(Form::_true()),
        _ => Err(Error::Bad),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod atoms {
        use super::*;

        #[test]
        fn nil() {
            assert_eq!(read_str2("nil"), Ok(Form::nil()));
        }

        #[test]
        fn _true() {
            assert_eq!(read_str2("true"), Ok(Form::_true()));
        }
    }
}
