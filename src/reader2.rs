use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, not_line_ending},
    combinator::{cut, map, opt, value},
    error::{context, ErrorKind as NomErrorKind, ParseError, VerboseError, VerboseErrorKind},
    multi::many_till,
    sequence::{preceded, tuple},
    AsChar, Err, IResult, InputTakeAtPosition,
};

use crate::{Comment, Form};

trait ParseErrorExt<I>: ParseError<I> {
    #[allow(unused_variables)]
    fn add_kind(i: I, context: &'static str, kind: ErrorKind, other: Self) -> Self {
        other
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Error<I> {
    error: VerboseError<I>,
    kind: ErrorKind,
}

impl<I> Error<I> {
    fn from_kind(input: I, kind: ErrorKind) -> Self {
        Error {
            error: VerboseError::from_error_kind(input, NomErrorKind::Eof),
            kind,
        }
    }
}

impl<I> ParseError<I> for Error<I> {
    fn from_error_kind(input: I, kind: NomErrorKind) -> Self {
        Error {
            error: VerboseError::from_error_kind(input, kind),
            kind: ErrorKind::Bad,
        }
    }

    fn append(input: I, nom_kind: NomErrorKind, other: Self) -> Self {
        let Error { error, kind } = other;
        Error {
            error: VerboseError::append(input, nom_kind, error),
            kind,
        }
    }
}

impl<I> ParseErrorExt<I> for Error<I>
where
    Error<I>: ParseError<I>,
{
    fn add_kind(input: I, context: &'static str, kind: ErrorKind, mut other: Self) -> Self {
        other
            .error
            .errors
            .push((input, VerboseErrorKind::Context(context)));
        other.kind = kind;
        other
    }
}

fn with_kind<I: Clone, E: ParseErrorExt<I>, F, O>(
    context: &'static str,
    kind: ErrorKind,
    f: F,
) -> impl Fn(I) -> IResult<I, O, E>
where
    F: Fn(I) -> IResult<I, O, E>,
{
    move |i: I| match f(i.clone()) {
        Ok(o) => Ok(o),
        Err(Err::Incomplete(i)) => Err(Err::Incomplete(i)),
        Err(Err::Error(e)) => Err(Err::Error(E::add_kind(i, context, kind, e))),
        Err(Err::Failure(e)) => Err(Err::Failure(E::add_kind(i, context, kind, e))),
    }
}

macro_rules! with_kind {
    ($kind:expr, $f:expr $(,)?) => {{
        with_kind(stringify!($kind), $kind, $f)
    }};
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ErrorKind {
    Bad,
    UnterminatedList,
}

fn whitespacechar(c: char) -> bool {
    match c {
        ' ' | ',' | '\t' | '\r' | '\n' => false,
        _ => true,
    }
}

fn whitespace0<T, E: ParseErrorExt<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position_complete(|item| whitespacechar(item.as_char()))
}

fn read_form_comment<'a, E: ParseErrorExt<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Comment, E> {
    context(
        "form_comment",
        map(
            preceded(tuple((tag("#_"), whitespace0)), read_form),
            |form: Form| Comment::Form(Box::new(form)),
        ),
    )(input)
}

fn read_line_comment<'a, E: ParseErrorExt<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Comment, E> {
    context(
        "line_comment",
        map(preceded(tag(";"), not_line_ending), |s: &'a str| {
            Comment::Line(String::from(s))
        }),
    )(input)
}

fn read_comment<'a, E: ParseErrorExt<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context(
        "read_comment",
        map(alt((read_form_comment, read_line_comment)), Form::Comment),
    )(input)
}

fn read_list<'a, E: ParseErrorExt<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context(
        "read_list",
        with_kind!(
            ErrorKind::UnterminatedList,
            map(
                preceded(char('('), cut(many_till(read_form, char(')')))),
                |(f, _)| Form::list(f)
            ),
        ),
    )(input)
}

fn read_form<'a, E: ParseErrorExt<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
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

fn opt_read_form<'a, E: ParseErrorExt<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Option<Form>, E> {
    opt(read_form)(input)
}

pub fn read_str2<'a>(input: &'a str) -> Result<Option<Form>, Error<&'a str>> {
    match opt_read_form::<self::Error<&'a str>>(input) {
        Ok((_, form)) => Ok(form),
        Err(e) => match e {
            Err::Incomplete(_i) => Err(self::Error::from_kind(input, ErrorKind::Bad)),
            Err::Error(e) | Err::Failure(e) => Err(e),
        },
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

            use nom::error::{
                ErrorKind::{Alt, ManyTill, Tag},
                VerboseErrorKind::{Context, Nom},
            };

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

            #[test]
            fn n_element() {
                assert_eq!(
                    read_str2("(true nil)"),
                    Ok(Some(Form::list(vec![Form::_true(), Form::nil()])))
                )
            }

            #[test]
            fn unterminated() {
                assert_eq!(
                    read_str2("(nil true"),
                    Err(Error {
                        error: VerboseError {
                            errors: vec![
                                ("", Nom(Tag)),
                                ("", Nom(Alt)),
                                ("", Nom(ManyTill)),
                                ("(nil true", Context("ErrorKind::UnterminatedList"))
                            ]
                        },
                        kind: ErrorKind::UnterminatedList,
                    })
                )
            }
        }
    }
}
