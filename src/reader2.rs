use crate::Form;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Error {
    Bad,
}

pub fn read_str2(_input: &str) -> Result<Form, Error> {
    Ok(Form::nil())
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
    }
}
