mod ast;
mod interpret;
mod parse;
mod scanner;
mod token;

use std::{fs::read_to_string, io::Write};

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

fn main() {
    let args = Args::parse();
    let source_text = read_to_string(args.file).expect("unable to read file");

    let mut stdout = std::io::stdout();
    let mut stderr = std::io::stderr();
    run(source_text, &mut stdout, &mut stderr);
}

fn run<W: Write, X: Write>(source: String, stdout: &mut W, stderr: &mut X) {
    let scanner = Scanner::new(source);
    let (tokens, errors) = scanner.scan_tokens();

    if errors.len() > 0 {
        for (error, line) in errors {
            writeln!(stderr, "[line {}] Error: {}", line, error).unwrap();
        }
        return;
    }

    let parser = Parser::new(tokens);
    let parsed = parser.parse();

    let stmts = match parsed {
        Ok(expr) => expr,
        Err(errors) => {
            report_parse_errors(errors, stderr);
            return;
        }
    };

    let wrapper = WriteWrapper { writer: stdout };
    let boxed_writer = Box::new(wrapper);
    let mut interpreter = Interpreter::new(boxed_writer);
    if let Err(err) = interpreter.interpret(&stmts) {
        writeln!(stderr, "{}", err).unwrap();
    }
}

struct WriteWrapper<'a, W: Write + ?Sized> {
    writer: &'a mut W,
}

impl<'a, W: Write + ?Sized> Write for WriteWrapper<'a, W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.writer.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_examples() {
        let examples_dir = std::fs::read_dir("examples").unwrap();

        for example in examples_dir {
            eprintln!("{:?}", example);
            let example = example.unwrap();
            let file_name = example.file_name();
            let file_name = file_name.to_str().unwrap();
            let source_text = read_to_string(format!("examples/{}", file_name)).unwrap();

            let mut stdout = Vec::new();
            let mut stderr = Vec::new();
            run(source_text, &mut stdout, &mut stderr);

            let contents = format!(
                "stdout:\n{}\nstderr:\n{}",
                String::from_utf8(stdout).unwrap(),
                String::from_utf8(stderr).unwrap()
            );

            insta::assert_snapshot!(file_name, contents);
        }
    }
}
