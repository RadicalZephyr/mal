use std::io;

use rustyline::error::ReadlineError;
use rustyline::Editor;

use mal::{pr_str, read_str, Form};

#[derive(Copy, Clone, Debug)]
enum Error {
    ReadError,
}

fn read(s: &str) -> Result<Form, Error> {
    read_str(s).map_err(|_| Error::ReadError)
}

fn eval(s: Form) -> Result<Form, Error> {
    Ok(s)
}

fn print(s: Form) -> String {
    pr_str(&s, true)
}

fn rep(s: String) -> Result<String, Error> {
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
                    match rep(line) {
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
