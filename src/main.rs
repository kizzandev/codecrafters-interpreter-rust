use std::env;
use std::process::ExitCode;

mod ast;
mod evaluator;
mod lexer;
mod parser;
mod tokenizer;

use crate::evaluator::evaluate;
use crate::parser::parse;
use crate::tokenizer::tokenize;
use interpreter_starter_rust::read_file;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return ExitCode::from(65);
    }

    let command = &args[1];
    let filename = &args[2];

    let file_contents = read_file(filename);

    return match command.as_str() {
        "tokenize" => tokenize(&file_contents),
        "parse" => {
            match parse(&file_contents) {
                Ok(expr) => {
                    println!("{}", expr.to_string());
                    ExitCode::SUCCESS
                }
                _ => ExitCode::from(65),
            }
        }
        "evaluate" => {
            let result = match parse(&file_contents) {
                Ok(expr) => {
                    let eval = evaluate(&expr);
                    if eval.is_runtime_error() {
                        eprintln!("{}", eval.to_string());
                        return ExitCode::from(70);
                    }
                    println!("{}", eval.to_string());
                    ExitCode::SUCCESS
                }
                _ => ExitCode::from(65),
            };
            result
        }
        _ => {
            eprintln!("Usage: {} <action> <filename>", args[0]);
            ExitCode::from(65)
        }
    };
}
