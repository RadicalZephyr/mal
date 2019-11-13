use std::str::FromStr;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, digit0, digit1, not_line_ending},
    combinator::{map, map_res, opt, recognize, value},
    error::{convert_error, ErrorKind, ParseError, VerboseError},
    multi::many0,
    sequence::{pair, preceded, terminated, tuple},
    AsChar, Err, FindSubstring, IResult, InputTakeAtPosition,
};

use rug::{Float, Integer};

use crate::{Atom, Bool, Form};

// -------------------- Utility Functions --------------------

#[allow(dead_code)]
fn comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    preceded(tag(";;"), not_line_ending)(input)
}

fn stringchar0<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    let bytes = input.as_bytes();
    let mut search_index = 0;

    while let Some(index) = (&bytes[search_index..]).find_substring("\"") {
        search_index += index;
        let prev_index = if search_index == 0 {
            0
        } else {
            search_index - 1
        };
        if b'\\' == bytes[prev_index] {
            // Skip escaped double quotations
            search_index += 1;
        } else {
            break;
        }
    }

    Ok((&input[search_index..], &input[..search_index]))
}

fn symbolchar(c: char) -> bool {
    match c {
        '"' | '\'' | '(' | ')' | '[' | ']' | '{' | '}' | '#' | ' ' | ',' | '\r' | '\n' | '\t' => {
            true
        }
        _ => false,
    }
}

#[allow(dead_code)]
fn symbolchar0<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position_complete(|item| symbolchar(item.as_char()))
}

fn symbolchar1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position1_complete(|item| symbolchar(item.as_char()), ErrorKind::Space)
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

#[allow(dead_code)]
fn whitespace1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
    T: InputTakeAtPosition,
    <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position1_complete(|item| whitespacechar(item.as_char()), ErrorKind::Space)
}

// -------------------- Atom Readers --------------------

fn read_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Atom, E> {
    map_res(
        recognize(tuple((read_integer, char('.'), digit0))),
        |s: &'a str| {
            s.parse::<f64>()
                .map(|v| Atom::Float(Float::with_val(12, v)))
        },
    )(input)
}

fn read_integer<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Atom, E> {
    map_res(recognize(preceded(opt(char('-')), digit1)), |s: &str| {
        s.parse::<Integer>().map(Atom::Integer)
    })(input)
}

fn read_nil<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Atom, E> {
    value(Atom::Nil, tag("nil"))(input)
}

fn read_string<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Atom, E> {
    map(
        preceded(char('"'), terminated(stringchar0, char('"'))),
        |s: &str| Atom::String(s.to_string()),
    )(input)
}

pub struct NotABoolean;

impl FromStr for Bool {
    type Err = NotABoolean;

    fn from_str(s: &str) -> Result<Bool, Self::Err> {
        match s {
            "true" => Ok(Bool::True),
            "false" => Ok(Bool::False),
            _ => Err(NotABoolean),
        }
    }
}

fn read_symbol<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Atom, E> {
    let (input, symbolname) = symbolchar1(input)?;
    if let Ok(boolean) = symbolname.parse::<Bool>() {
        return Ok((input, Atom::Bool(boolean)));
    }
    if symbolname.starts_with(':') {
        Ok((input, Atom::Keyword(symbolname.to_string())))
    } else {
        Ok((input, Atom::Symbol(symbolname.to_string())))
    }
}

// -------------------- Form Readers --------------------

fn read_atom<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    map(
        alt((read_float, read_integer, read_string, read_nil, read_symbol)),
        Form::Atom,
    )(input)
}

fn read_list<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    preceded(
        pair(whitespace0, tag("(")),
        terminated(
            map(many0(read_form), Form::List),
            tuple((whitespace0, tag(")"))),
        ),
    )(input)
}

fn read_form<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, Form, E> {
    preceded(whitespace0, alt((read_list, read_atom)))(input)
}

pub fn read_str(input: &str) -> Result<Form, ()> {
    match read_form::<VerboseError<&str>>(input) {
        Ok((_, form)) => Ok(form),
        Err(Err::Incomplete(x)) => {
            eprintln!("INCOMPLETE parse: {:?}", x);
            Err(())
        }
        Err(Err::Error(e)) | Err(Err::Failure(e)) => {
            eprintln!("ERROR: {:?}", convert_error(input, e));
            Err(())
        }
    }
}
