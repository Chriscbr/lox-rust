mod ast;
mod error;
mod parse;
mod scanner;
mod token;

use std::fs::read_to_string;

use clap::Parser as ClapParser;
use error::Error;
use parse::Parser;
use scanner::Scanner;
use token::TokenType;

/// Run a Lox program.
#[derive(ClapParser, Debug)]
#[command(version, about)]
struct Args {
    /// File path
    file: String,
}

fn run(source: String) {
    let scanner = Scanner::new(source);
    let (tokens, errors) = scanner.scan_tokens();

    if errors.len() > 0 {
        report_errors(&errors);
        return;
    }

    let mut parser = Parser::new(tokens);
    let parsed = parser.parse();

    match parsed {
        Ok(expr) => println!("{}", expr),
        Err(err) => report_errors(&[err]),
    };
}

fn report_errors(errors: &[Error]) {
    for error in errors {
        match error {
            Error::UnexpectedCharacter { line } => {
                println!("[line {}] Error: unexpected character.", line)
            }
            Error::UnterminatedString { line } => {
                println!("[line {}] Error: unterminated string.", line)
            }
            Error::ExpectRightParenAfterExpression { token } => match token.ty {
                TokenType::EOF => println!(
                    "[line {}] Error at end: Expected ')' after expression.",
                    token.line
                ),
                _ => println!(
                    "[line {}] Error at '{}': Expected ')' after expression.",
                    token.line, token.lexeme,
                ),
            },
        }
    }
}

fn main() {
    let args = Args::parse();
    let source_text = read_to_string(args.file).expect("unable to read file");
    run(source_text);
}
