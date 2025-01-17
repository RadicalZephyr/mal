use derive_more::Display;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit1, not_line_ending},
    combinator::{cut, map, opt, recognize, value},
    error::{context, ErrorKind as NomErrorKind, ParseError, VerboseError, VerboseErrorKind},
    multi::many_till,
    number::complete::recognize_float,
    sequence::{delimited, pair, preceded, separated_pair, tuple},
    AsChar, Err, IResult, InputTakeAtPosition,
};

use rug::{
    Complex as RugComplex, Float as RugFloat, Integer as RugInteger, Rational as RugRational,
};

use crate::{Comment, Form};

trait ParseErrorExt<I>: ParseError<I> {
    #[allow(unused_variables)]
    fn add_kind(i: I, context: &'static str, kind: ErrorKind, other: Self) -> Self {
        other
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug, Display, PartialEq)]
pub enum ErrorKind {
    #[display(fmt = "this file contains an un-closed delimiter: `{}`", _0)]
    UnclosedDelimiter(char),

    #[display(fmt = "incomplete parse")]
    Incomplete,

    #[display(fmt = "incorrect close delimiter: `{}`", _0)]
    IncorrectCloseDelimiter(char),

    #[display(fmt = "unexpected close delimiter: `{}`", _0)]
    UnexpectedCloseDelimiter(char),

    #[display(fmt = "uneven number of elements found in map")]
    UnevenNumberOfMapElements,

    #[display(fmt = "verbose error {:?}", _0)]
    Verbose(VerboseErrorKind),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Error<I> {
    error: VerboseError<I>,
    kind: Option<ErrorKind>,
}

impl<I> Error<I> {
    fn from_kind(input: I, kind: ErrorKind) -> Self {
        Error {
            error: VerboseError::from_error_kind(input, NomErrorKind::Eof),
            kind: Some(kind),
        }
    }
}

impl<I> ParseError<I> for Error<I> {
    fn from_error_kind(input: I, kind: NomErrorKind) -> Self {
        Error {
            error: VerboseError::from_error_kind(input, kind),
            kind: None,
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
        if other.kind.is_none() {
            other
                .error
                .errors
                .push((input, VerboseErrorKind::Context(context)));
            other.kind = Some(kind);
        }
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
        Err(Err::Error(e)) => Err(Err::Error(E::add_kind(i, context, kind.clone(), e))),
        Err(Err::Failure(e)) => Err(Err::Failure(E::add_kind(i, context, kind.clone(), e))),
    }
}

macro_rules! with_kind {
    ($kind:expr, $f:expr $(,)?) => {{
        with_kind(stringify!($kind), $kind, $f)
    }};
}

fn whitespacechar(c: char) -> bool {
    match c {
        ' ' | ',' | '\t' | '\r' | '\n' => false,
        _ => true,
    }
}

fn string0<T, E: ParseErrorExt<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position_complete(|item| match item.as_char() {
        '"' => true,
        _ => false,
    })
}

fn not_symbolchar(c: char) -> bool {
    match c {
        '"' | '\'' | '(' | ')' | '[' | ']' | '{' | '}' | '#' | ' ' | ',' | '\r' | '\n' | '\t' => {
            true
        }
        _ => false,
    }
}

#[allow(dead_code)]
fn symbolchar0<T, E>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
    E: ParseErrorExt<T>,
{
    input.split_at_position_complete(|item| not_symbolchar(item.as_char()))
}

fn symbolchar1<T, E: ParseErrorExt<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
    E: ParseErrorExt<T>,
{
    input.split_at_position1_complete(|item| not_symbolchar(item.as_char()), NomErrorKind::Space)
}

fn whitespace0<T, E: ParseErrorExt<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position_complete(|item| whitespacechar(item.as_char()))
}

fn context_map<'a, I, O, E, F, G>(
    ctx: &'static str,
    recognizer: F,
    form_constructor: G,
) -> impl Fn(&'a str) -> IResult<&'a str, O, E>
where
    E: ParseErrorExt<&'a str>,
    F: Fn(&'a str) -> IResult<&'a str, I, E>,
    G: Fn(I) -> O,
{
    context(ctx, map(recognizer, form_constructor))
}

fn read_form_comment<'a, E: ParseErrorExt<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Comment, E> {
    context_map(
        "form_comment",
        preceded(tuple((tag("#_"), whitespace0)), read_form),
        |form: Form| Comment::Form(Box::new(form)),
    )(input)
}

fn read_line_comment<'a, E: ParseErrorExt<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Comment, E> {
    context_map(
        "line_comment",
        preceded(tag(";"), not_line_ending),
        |s: &'a str| Comment::Line(String::from(s)),
    )(input)
}

fn read_comment<'a, E: ParseErrorExt<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context_map(
        "read_comment",
        alt((read_form_comment, read_line_comment)),
        Form::Comment,
    )(input)
}

fn read_delimited_form_seq<'a, C, I, F, E: ParseErrorExt<&'a str>>(
    ctx: &'static str,
    constructor: C,
    open_delimiter: char,
    item: F,
    close_delimiter: char,
) -> impl Fn(&'a str) -> IResult<&'a str, Form, E>
where
    C: Fn(Vec<I>) -> Form,
    F: Fn(&'a str) -> IResult<&'a str, I, E>,
{
    move |input: &str| {
        context_map(
            ctx,
            preceded(
                char(open_delimiter),
                with_kind!(
                    ErrorKind::UnclosedDelimiter(close_delimiter),
                    cut(many_till(
                        &item,
                        preceded(whitespace0, char(close_delimiter))
                    ))
                ),
            ),
            |(f, _)| constructor(f),
        )(input)
    }
}

fn read_list<'a, E: ParseErrorExt<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    read_delimited_form_seq("read_list", Form::list, '(', read_form, ')')(input)
}

fn read_map<'a, E: ParseErrorExt<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    read_delimited_form_seq(
        "read_list",
        Form::map,
        '{',
        pair(
            read_form,
            with_kind!(ErrorKind::UnevenNumberOfMapElements, cut(read_form)),
        ),
        '}',
    )(input)
}

fn read_vector<'a, E: ParseErrorExt<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    read_delimited_form_seq("read_vector", Form::vector, '[', read_form, ']')(input)
}

// ---------------------------------------------
// ---  Numbers
// ---------------------------------------------

fn read_complex<'a, E: ParseErrorExt<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context_map(
        "read_complex",
        recognize(delimited(
            char('('),
            separated_pair(recognize_float, char(','), recognize_float),
            char(')'),
        )),
        |i: &str| Form::complex(RugComplex::with_val(53, RugComplex::parse(i).unwrap())),
    )(input)
}

fn read_float<'a, E: ParseErrorExt<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context_map(
        "read_float",
        recognize(tuple((digit1, char('.'), digit1))),
        |i: &str| Form::float(RugFloat::with_val(53, RugFloat::parse(i).unwrap())),
    )(input)
}

fn read_integer<'a, E: ParseErrorExt<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context_map(
        "read_integer",
        recognize(tuple((opt(char('-')), digit1))),
        |i: &str| Form::integer(i.parse::<RugInteger>().unwrap()),
    )(input)
}

fn read_rational<'a, E: ParseErrorExt<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context_map(
        "read_rational",
        recognize(tuple((digit1, char('/'), digit1))),
        |i: &str| Form::rational(i.parse::<RugRational>().unwrap()),
    )(input)
}

fn read_reader_macros<'a, E: ParseErrorExt<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context(
        "read_reader_macros",
        preceded(char('#'), preceded(char('i'), read_complex)),
    )(input)
}

fn read_string<'a, E: ParseErrorExt<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context_map(
        "read_string",
        delimited(char('"'), string0, char('"')),
        Form::string,
    )(input)
}

fn read_symbol<'a, E: ParseErrorExt<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context_map("read_symbol", symbolchar1, Form::symbol)(input)
}

fn read_symbol_like_atoms<'a, E: ParseErrorExt<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Form, E> {
    alt((
        value(Form::nil(), tag("nil")),
        value(Form::_true(), tag("true")),
        value(Form::_false(), tag("false")),
        read_symbol,
    ))(input)
}

fn read_form<'a, E: ParseErrorExt<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    context(
        "read_form",
        preceded(
            whitespace0,
            alt((
                read_comment,
                read_list,
                read_map,
                read_vector,
                read_reader_macros,
                read_string,
                read_float,
                read_rational,
                read_integer,
                read_symbol_like_atoms,
            )),
        ),
    )(input)
}

fn opt_read_form<'a, E: ParseErrorExt<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Option<Form>, E> {
    opt(read_form)(input)
}

pub fn read_str<'a>(input: &'a str) -> Result<Option<Form>, Error<&'a str>> {
    match opt_read_form::<self::Error<&'a str>>(input) {
        Ok((_, form)) => Ok(form),
        Err(e) => match e {
            Err::Incomplete(_i) => Err(self::Error::from_kind(input, ErrorKind::Incomplete)),
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
            assert_eq!(read_str("  nil"), Ok(Some(Form::nil())));
        }
    }

    mod comments {
        use super::*;

        use crate::Comment;

        #[test]
        fn line() {
            assert_eq!(
                read_str("; Comment"),
                Ok(Some(Form::Comment(Comment::Line(" Comment".into()))))
            );
        }

        #[test]
        fn form() {
            assert_eq!(
                read_str("#_nil"),
                Ok(Some(Form::Comment(Comment::Form(Box::new(Form::nil())))))
            )
        }
    }

    mod atoms {
        use super::*;

        #[test]
        fn nil() {
            assert_eq!(read_str("nil"), Ok(Some(Form::nil())));
        }

        #[test]
        fn _true() {
            assert_eq!(read_str("true"), Ok(Some(Form::_true())));
        }

        #[test]
        fn _false() {
            assert_eq!(read_str("false"), Ok(Some(Form::_false())));
        }

        mod complex {
            use super::*;

            #[test]
            fn zero() {
                assert_eq!(
                    read_str("#i(1.0,1.0)"),
                    Ok(Some(Form::complex((1.0f32, 1.0f32))))
                );
            }
        }

        mod integer {
            use super::*;

            #[test]
            fn zero() {
                assert_eq!(read_str("0"), Ok(Some(Form::integer(0u8))));
            }

            #[test]
            fn one_hundred() {
                assert_eq!(read_str("100"), Ok(Some(Form::integer(100u8))));
            }

            #[test]
            fn negative_one() {
                assert_eq!(read_str("-1"), Ok(Some(Form::integer(-1i8))));
            }
        }

        mod float {
            use super::*;

            #[test]
            fn zero() {
                assert_eq!(read_str("0.0"), Ok(Some(Form::float(0.0f32))));
            }

            #[test]
            fn one_hundred() {
                assert_eq!(read_str("100.0"), Ok(Some(Form::float(100.0f32))));
            }
        }

        mod rational {
            use super::*;

            #[test]
            fn zero() {
                assert_eq!(read_str("0/1"), Ok(Some(Form::rational((0, 1)))));
            }

            #[test]
            fn one_hundred() {
                assert_eq!(read_str("200/2"), Ok(Some(Form::rational((100, 1)))));
            }
        }

        mod string {
            use super::*;

            #[test]
            fn empty() {
                assert_eq!(read_str("\"\""), Ok(Some(Form::string(""))));
            }

            #[test]
            fn letters() {
                assert_eq!(read_str("\"abc\""), Ok(Some(Form::string("abc"))));
            }
        }

        mod symbol {
            use super::*;

            #[test]
            fn letters() {
                assert_eq!(read_str("abc"), Ok(Some(Form::symbol("abc"))));
            }

            #[test]
            fn numbers() {
                assert_eq!(read_str("a123"), Ok(Some(Form::symbol("a123"))));
            }

            #[test]
            fn special_chars() {
                assert_eq!(
                    read_str("z!@$%^&*-_+=|"),
                    Ok(Some(Form::symbol("z!@$%^&*-_+=|")))
                );
            }
        }
    }

    mod compound {
        use super::*;

        mod list {
            use super::*;

            use nom::error::{
                ErrorKind::{Alt, ManyTill, Space},
                VerboseErrorKind::{Context, Nom},
            };

            #[test]
            fn empty() {
                assert_eq!(read_str("()"), Ok(Some(Form::empty_list())));
            }

            #[test]
            fn one_element() {
                assert_eq!(
                    read_str("(nil)"),
                    Ok(Some(Form::list(std::iter::once(Form::nil()))))
                );
            }

            #[test]
            fn n_element() {
                assert_eq!(
                    read_str("(true nil)"),
                    Ok(Some(Form::list(vec![Form::_true(), Form::nil()])))
                )
            }

            #[test]
            fn unterminated() {
                assert_eq!(
                    read_str("(nil true"),
                    Err(Error {
                        error: VerboseError {
                            errors: vec![
                                ("", Nom(Space)),
                                ("", Nom(Alt)),
                                ("", Nom(Alt)),
                                ("", Nom(ManyTill)),
                                (
                                    "nil true",
                                    Context("ErrorKind::UnclosedDelimiter(close_delimiter)")
                                )
                            ]
                        },
                        kind: Some(ErrorKind::UnclosedDelimiter(')')),
                    })
                )
            }
        }

        mod map {
            use super::*;

            use nom::error::{
                ErrorKind::{Alt, ManyTill, Space},
                VerboseErrorKind::{Context, Nom},
            };

            #[test]
            fn empty() {
                assert_eq!(read_str("{}"), Ok(Some(Form::empty_map())));
            }

            #[test]
            fn one_kv() {
                assert_eq!(
                    read_str("{true nil}"),
                    Ok(Some(Form::map(vec![(Form::_true(), Form::nil())])))
                );
            }

            #[test]
            fn n_kv() {
                assert_eq!(
                    read_str("{true nil, nil true}"),
                    Ok(Some(Form::map(vec![
                        (Form::_true(), Form::nil()),
                        (Form::nil(), Form::_true())
                    ])))
                );
            }

            #[test]
            fn unterminated() {
                assert_eq!(
                    read_str("{nil true"),
                    Err(Error {
                        error: VerboseError {
                            errors: vec![
                                ("", Nom(Space)),
                                ("", Nom(Alt)),
                                ("", Nom(Alt)),
                                ("", Nom(ManyTill)),
                                (
                                    "nil true",
                                    Context("ErrorKind::UnclosedDelimiter(close_delimiter)")
                                )
                            ]
                        },
                        kind: Some(ErrorKind::UnclosedDelimiter('}')),
                    })
                )
            }

            #[test]
            fn uneven_number_of_elements() {
                assert_eq!(
                    read_str("{true nil nil}"),
                    Err(Error {
                        error: VerboseError {
                            errors: vec![
                                ("}", Nom(Space)),
                                ("}", Nom(Alt)),
                                ("}", Nom(Alt)),
                                ("}", Context("ErrorKind::UnevenNumberOfMapElements"))
                            ]
                        },
                        kind: Some(ErrorKind::UnevenNumberOfMapElements),
                    })
                )
            }
        }

        mod vector {
            use super::*;

            use nom::error::{
                ErrorKind::{Alt, ManyTill, Space},
                VerboseErrorKind::{Context, Nom},
            };

            #[test]
            fn empty() {
                assert_eq!(read_str("[]"), Ok(Some(Form::empty_vector())));
            }

            #[test]
            fn one_element() {
                assert_eq!(
                    read_str("[nil]"),
                    Ok(Some(Form::vector(std::iter::once(Form::nil()))))
                );
            }

            #[test]
            fn n_element() {
                assert_eq!(
                    read_str("[true nil]"),
                    Ok(Some(Form::vector(vec![Form::_true(), Form::nil()])))
                )
            }

            #[test]
            fn unterminated() {
                assert_eq!(
                    read_str("[nil true"),
                    Err(Error {
                        error: VerboseError {
                            errors: vec![
                                ("", Nom(Space)),
                                ("", Nom(Alt)),
                                ("", Nom(Alt)),
                                ("", Nom(ManyTill)),
                                (
                                    "nil true",
                                    Context("ErrorKind::UnclosedDelimiter(close_delimiter)")
                                )
                            ]
                        },
                        kind: Some(ErrorKind::UnclosedDelimiter(']')),
                    })
                )
            }
        }
    }
}
