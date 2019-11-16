use std::io;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use mal::{pr_str, read_str, Form, ReaderError2 as MalError};

#[derive(Clone, Debug)]
enum Error<'a> {
    NothingRead,
    ReadError,
    Mal(MalError<&'a str>),
}

impl<'a> From<MalError<&'a str>> for Error<'a> {
    fn from(error: MalError<&'a str>) -> Error<'a> {
        Error::Mal(error)
    }
}

fn read(s: &str) -> Result<Form, Error> {
    read_str(s)?
        .ok_or(Error::NothingRead)
        .map_err(|_| Error::ReadError)
}

fn eval<'a>(s: Form) -> Result<Form, Error<'a>> {
    Ok(s)
}

fn print(s: Form) -> String {
    pr_str(&s, true)
}

fn rep<'a>(s: &'a String) -> Result<String, Error<'a>> {
    Ok(print(eval(read(&s)?)?))
}

fn main() -> io::Result<()> {
    let mut rl = Editor::<()>::new();
    if rl.load_history(".mal-history").is_err() {
        eprintln!("No previous history.");
    }

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                rl.save_history(".mal-history").ok();
                if line.len() > 0 {
                    match rep(&line) {
                        Ok(output) => println!("{}", output),
                        Err(e) => eprintln!("{:?}", e),
                    }
                }
            }

            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }

    eprintln!("\nBye!");
    Ok(())
}
