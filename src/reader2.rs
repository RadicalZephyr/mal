use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::value,
    error::{ParseError, VerboseError},
    sequence::preceded,
    AsChar, IResult, InputTakeAtPosition,
};

use crate::Form;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Error {
    Bad,
}

fn whitespacechar(c: char) -> bool {
    match c {
        ' ' | ',' | '\t' | '\r' | '\n' => false,
        _ => true,
    }
}

fn whitespace0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position_complete(|item| whitespacechar(item.as_char()))
}

pub fn read_form<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    preceded(
        whitespace0,
        alt((
            value(Form::nil(), tag("nil")),
            value(Form::_true(), tag("true")),
        )),
    )(input)
}

pub fn read_str2(input: &str) -> Result<Form, Error> {
    match read_form::<VerboseError<&str>>(input) {
        Ok((_, form)) => Ok(form),
        _ => Err(Error::Bad),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod whitespace {
        use super::*;

        #[test]
        fn leading_whitespace() {
            assert_eq!(read_str2("  nil"), Ok(Form::nil()));
        }
    }

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
