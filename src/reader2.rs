use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::not_line_ending,
    combinator::{map, opt, value},
    error::{context, ParseError, VerboseError},
    sequence::preceded,
    AsChar, IResult, InputTakeAtPosition,
};

use crate::{Comment, Form};

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

fn read_comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context(
        "comment",
        map(preceded(tag(";"), not_line_ending), |s: &'a str| {
            Form::Comment(Comment::Line(String::from(s)))
        }),
    )(input)
}

fn read_form<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    preceded(
        whitespace0,
        alt((
            read_comment,
            value(Form::nil(), tag("nil")),
            value(Form::_true(), tag("true")),
        )),
    )(input)
}

fn opt_read_form<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Option<Form>, E> {
    opt(read_form)(input)
}

pub fn read_str2(input: &str) -> Result<Option<Form>, Error> {
    match opt_read_form::<VerboseError<&str>>(input) {
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
            assert_eq!(read_str2("  nil"), Ok(Some(Form::nil())));
        }
    }

    mod comments {
        use super::*;

        use crate::Comment;

        #[test]
        fn line() {
            assert_eq!(
                read_str2("; Comment"),
                Ok(Some(Form::Comment(Comment::Line(" Comment".into()))))
            );
        }
    }

    mod atoms {
        use super::*;

        #[test]
        fn nil() {
            assert_eq!(read_str2("nil"), Ok(Some(Form::nil())));
        }

        #[test]
        fn _true() {
            assert_eq!(read_str2("true"), Ok(Some(Form::_true())));
        }
    }
}
