use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, not_line_ending},
    combinator::{map, opt, value},
    error::{context, ParseError, VerboseError},
    multi::many_till,
    sequence::{preceded, tuple},
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
fn read_form_comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Comment, E> {
    context(
        "form_comment",
        map(
            preceded(tuple((tag("#_"), whitespace0)), read_form),
            |form: Form| Comment::Form(Box::new(form)),
        ),
    )(input)
}

fn read_line_comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Comment, E> {
    context(
        "line_comment",
        map(preceded(tag(";"), not_line_ending), |s: &'a str| {
            Comment::Line(String::from(s))
        }),
    )(input)
}

fn read_comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context(
        "read_comment",
        map(alt((read_form_comment, read_line_comment)), Form::Comment),
    )(input)
}

fn read_list<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context(
        "read_list",
        map(
            preceded(char('('), many_till(read_form, char(')'))),
            |(f, _)| Form::list(f),
        ),
    )(input)
}

fn read_form<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context(
        "read_form",
        preceded(
            whitespace0,
            alt((
                read_comment,
                read_list,
                value(Form::nil(), tag("nil")),
                value(Form::_true(), tag("true")),
            )),
        ),
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

        #[test]
        fn form() {
            assert_eq!(
                read_str2("#_nil"),
                Ok(Some(Form::Comment(Comment::Form(Box::new(Form::nil())))))
            )
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

    mod compound {
        use super::*;

        mod list {
            use super::*;

            #[test]
            fn empty() {
                assert_eq!(read_str2("()"), Ok(Some(Form::empty_list())));
            }

            #[test]
            fn one_element() {
                assert_eq!(
                    read_str2("(nil)"),
                    Ok(Some(Form::list(std::iter::once(Form::nil()))))
                );
            }
        }
    }
}
