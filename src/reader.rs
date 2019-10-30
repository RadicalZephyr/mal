use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, anychar, char, digit1, not_line_ending},
    combinator::{all_consuming, map, map_res, recognize, value},
    error::{ErrorKind, ParseError, VerboseError},
    multi::{many0, many_till, separated_list},
    sequence::{preceded, tuple},
    AsChar, IResult, InputTakeAtPosition,
};

use num_bigint::BigInt;

use crate::{Atom, Form};

fn comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    preceded(tag(";;"), not_line_ending)(input)
}

pub fn whitespace1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar + Clone,
{
    input.split_at_position1_complete(
        |item| {
            let c = item.as_char();
            match c {
                ' ' | ',' | '\t' | '\r' | '\n' => false,
                _ => true,
            }
        },
        ErrorKind::Space,
    )
}

fn read_integer<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Atom, E> {
    map_res(digit1, |s: &str| s.parse::<BigInt>().map(Atom::Integer))(input)
}

fn read_keyword<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Atom, E> {
    map(
        recognize(tuple((tag(":"), many0(alphanumeric1)))),
        |s: &str| Atom::Keyword(s.to_string()),
    )(input)
}

fn read_string<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Atom, E> {
    preceded(
        char('"'),
        map(recognize(many_till(anychar, char('"'))), |s: &str| {
            Atom::Symbol(s.to_string())
        }),
    )(input)
}

fn read_symbol<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Atom, E> {
    map(
        recognize(tuple((alpha1, many0(alphanumeric1)))),
        |s: &str| Atom::Symbol(s.to_string()),
    )(input)
}

fn read_nil<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Atom, E> {
    value(Atom::Nil, tag("nil"))(input)
}

fn read_atom<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    map(
        alt((
            read_integer,
            read_keyword,
            read_string,
            read_nil,
            read_symbol,
        )),
        Form::Atom,
    )(input)
}

fn read_list<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    let (input, _) = tag("(")(input)?;
    let (input, ret) = map(separated_list(whitespace1, read_form), Form::List)(input)?;
    let (input, _) = tag(")")(input)?;
    Ok((input, ret))
}

fn read_form<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    alt((read_list, read_atom))(input)
}

pub fn read_str(input: &str) -> Result<Form, ()> {
    match all_consuming(read_form::<VerboseError<&str>>)(input) {
        Ok((_, form)) => Ok(form),
        Err(_) => Err(()),
    }
}
