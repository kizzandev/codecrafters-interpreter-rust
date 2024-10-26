use std::borrow::Borrow;
use std::env;
use std::process::ExitCode;

mod ast;
mod evaluator;
mod interpreter;
mod lexer;
mod parser;
mod tokenizer;

// use crate::evaluator::evaluate;
use crate::evaluator::eval;
// use crate::interpreter::interpret;
// use crate::parser::{parse, ParseOption};
// use crate::parser::{print_expr, Parser};
use crate::parser::Parser;
use crate::tokenizer::tokenize;
use evaluator::Interpreter;
use interpreter_starter_rust::read_file;
// use parser::Stmt;

/*fn call_parse(file_contents: &str, option: ParseOption) -> ExitCode {
    match parse(&file_contents, option) {
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
        Err(err) => {
            // print if vector is some and has values
            if err.1.is_some() {
                for e in err.1.unwrap() {
                    println!("{}", e.to_string());
                }
            }

            return err.0;
        }
        // _ => ExitCode::from(65),
    }
}*/

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

            let a = parser.next();
            if a.is_some() {
                let a = a.unwrap();
                if a.is_ok() {
                    let a = a.unwrap();
                    eval(&a);
                }
            }

            ExitCode::SUCCESS
            /*let expr = parser.parse();

            match expr {
                Ok(expr) => {
                    for e in &expr {
                        let eval = eval(e);
                        if eval.is_runtime_error() {
                            eprintln!("{}", eval.get_error());
                            return ExitCode::from(70);
                        }
                        println!("{}", eval.to_string());
                    }
                    ExitCode::SUCCESS
                }
                _ => ExitCode::from(65),
            }*/
        }
        "run" => {
            let mut parser = Parser::new(&file_contents);
            let mut interpreter = Interpreter::new();

            while let Some(stmt) = parser.next() {
                match stmt {
                    Err(err) => {
                        eprintln!("{err}");
                        return ExitCode::from(65);
                    }
                    Ok(s) => {
                        // let stdout = run(s);
                        // let s: &parser::Stmt<'static> = &s.borrow();
                        let int = interpreter.run(s);
                        match int {
                            Err(e) => eprintln!("{e}"),
                            Ok(stdout) => println!("{stdout}"),
                        };

                        // let stdout = interpreter
                        //     .run(stmt.unwrap_or("Bad Statement.".to_string()))
                        //     .unwrap_or("Something went wrong.".to_string());
                        // match stdout {
                        //     Ok(s) => println!("{s}"),
                        //     Err(err) => eprintln!("{err}"),
                        // }
                        // println!("{stdout}");
                    }
                }
            }

            ExitCode::SUCCESS

            /*let expr = parser.parse();

            match expr {
                Ok(expr) => {
                    for e in &expr {
                        let eval = eval(e);
                        if eval.is_runtime_error() {
                            eprintln!("{}", eval.get_error());
                            return ExitCode::from(70);
                        }
                        println!("{}", eval.to_string());
                    }
                    ExitCode::SUCCESS
                }
                _ => ExitCode::from(65),
            }*/
        }

        /*"parse" => match parse(&file_contents, ParseOption::PARSE) {
            Ok(expr) => {
                println!("{}", expr[0].to_string());
                ExitCode::SUCCESS
            }
            _ => ExitCode::from(65),
        },
        "evaluate" => call_parse(&file_contents, ParseOption::EVALUATE),
        "run" => call_parse(&file_contents, ParseOption::RUN),*/
        // "evaluate" | "run" => match parse(&file_contents) {
        //     Ok(expr) => {
        //         for e in &expr {
        //             let eval = evaluate(&e);
        //             if eval.is_runtime_error() {
        //                 eprintln!("{}", eval.to_string());
        //                 return ExitCode::from(70);
        //             }
        //             println!("{}", eval.to_string());
        //         }

        //         ExitCode::SUCCESS
        //     }
        //     _ => ExitCode::from(65),
        // },
        // "run" => interpret(&file_contents),
        _ => {
            eprintln!("Usage: {} <action> <filename>", args[0]);
            ExitCode::from(65)
        }
    };
}
