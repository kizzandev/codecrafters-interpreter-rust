use std::env;
use std::process::ExitCode;

mod ast;
mod evaluator;
mod interpreter;
mod lexer;
mod parser;
mod tokenizer;

use crate::evaluator::evaluate;
// use crate::interpreter::interpret;
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
        "parse" => match parse(&file_contents) {
            Ok(expr) => {
                println!("{}", expr[0].to_string());
                ExitCode::SUCCESS
            }
            _ => ExitCode::from(65),
        },
        "evaluate" | "run" => match parse(&file_contents) {
            Ok(expr) => {
                for e in &expr {
                    let eval = evaluate(&e);
                    if eval.is_runtime_error() {
                        eprintln!("{}", eval.to_string());
                        return ExitCode::from(70);
                    }
                    println!("{}", eval.to_string());
                }

                ExitCode::SUCCESS
            }
            _ => ExitCode::from(65),
        },
        // "run" => interpret(&file_contents),
        _ => {
            eprintln!("Usage: {} <action> <filename>", args[0]);
            ExitCode::from(65)
        }
    };
}
