use std::env;
use std::process::ExitCode;

mod ast;
mod evaluator;
mod interpreter;
mod lexer;
mod parser;
mod tokenizer;

use crate::evaluator::eval;
use crate::parser::Parser;
use crate::tokenizer::tokenize;
use evaluator::Interpreter;
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
            let mut parser = Parser::new(&file_contents);

            while let Some(stmt) = parser.next() {
                match stmt {
                    Err(err) => {
                        eprintln!("{err}");
                        return ExitCode::from(65);
                    }
                    Ok(stmt_) => {
                        println!("{:?}", stmt_);
                    }
                }
            }

            ExitCode::SUCCESS
        }
        "evaluate" => {
            let mut parser = Parser::new(&file_contents);

            match parser.eval_expr() {
                Err(err) => {
                    eprintln!("Error: {err}");
                    return ExitCode::from(70);
                }
                Ok(s) => {
                    let res = eval(&s);
                    if res.is_runtime_error() {
                        eprintln!("Error: {:?}", res.get_error());
                        return ExitCode::from(70);
                    }
                }
            }

            // if a.is_some() {
            //     let a = a.unwrap();
            //     if a.is_ok() {
            //         let a = a.unwrap();
            //         eval(&a);
            //     }
            // }

            ExitCode::SUCCESS
        }
        "run" => {
            let mut parser = Parser::new(&file_contents);
            let mut interpreter = Interpreter::new();

            while let Some(stmt) = parser.next() {
                //  eprintln!("MAIN RUN STMT: {:?}", stmt);
                match stmt {
                    Err(err) => {
                        eprintln!("{err}");
                        return ExitCode::from(65);
                    }
                    Ok(s) => {
                        let res = interpreter.run(s);
                        if res.is_err() {
                            eprintln!("{:?}", res.err().unwrap());
                            return ExitCode::from(70);
                        }
                    }
                }
            }

            ExitCode::SUCCESS
        }
        _ => {
            eprintln!("Usage: {} <action> <filename>", args[0]);
            ExitCode::from(65)
        }
    };
}
