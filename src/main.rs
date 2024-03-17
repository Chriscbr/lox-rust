mod ast;
mod interpret;
mod parse;
mod scanner;
mod token;

use std::fs::read_to_string;

use clap::Parser as ClapParser;
use interpret::Interpreter;
use parse::{report_parse_errors, Parser};
use scanner::Scanner;

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
        for (error, line) in errors {
            println!("[line {}] Error: {}", line, error);
        }
        return;
    }

    let parser = Parser::new(tokens);
    let parsed = parser.parse();

    let stmts = match parsed {
        Ok(expr) => expr,
        Err(errors) => {
            report_parse_errors(errors);
            return;
        }
    };

    let mut interpreter = Interpreter::new();
    if let Err(err) = interpreter.interpret(&stmts) {
        println!("{}", err);
    }
}

fn main() {
    let args = Args::parse();
    let source_text = read_to_string(args.file).expect("unable to read file");
    run(source_text);
}
