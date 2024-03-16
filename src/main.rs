mod error;
mod scanner;
mod token;

use std::fs::read_to_string;

use clap::Parser;
use error::Error;
use scanner::Scanner;

/// Run a Lox program.
#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    /// File path
    file: String,
}

fn run(source: String) -> Result<(), Error> {
    let scanner = Scanner::new(source);
    let (tokens, errors) = scanner.scan_tokens();

    for token in tokens {
        println!("{:?}", token);
    }

    for error in errors {
        match error {
            Error::UnexpectedCharacter { line } => {
                println!("[line {}] Error: unexpected character.", line)
            }
            Error::UnterminatedString { line } => {
                println!("[line {}] Error: unterminated string.", line)
            }
        }
    }

    Ok(())
}

fn main() {
    let args = Args::parse();
    let source_text = read_to_string(args.file).expect("unable to read file");
    match run(source_text) {
        Ok(_) => {}
        Err(error) => {
            eprintln!("{:?}", error);
            std::process::exit(1);
        }
    }
}
