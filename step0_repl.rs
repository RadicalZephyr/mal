use std::io::{self, BufRead, Write};

fn read(s: String) -> String {
    s
}

fn eval(s: String) -> String {
    s
}

fn print(s: String) -> String {
    s
}

fn rep(s: String) -> String {
    print(eval(read(s)))
}

fn main() -> io::Result<()> {
    let mut buffer = String::new();

    let stdin_handle = std::io::stdin();
    let mut stdin = stdin_handle.lock();
    let stdout_handle = std::io::stdout();
    let mut stdout = stdout_handle.lock();

    loop {
        write!(stdout, "user> ")?;
        stdout.flush()?;
        if 0 == stdin.read_line(&mut buffer)? {
            writeln!(stdout, "\nBye!")?;
            break Ok(());
        }
        write!(stdout, "{}", rep(buffer.clone()))?;
        buffer.clear();
    }
}
