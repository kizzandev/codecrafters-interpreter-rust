use std::env;
use std::process::exit;

mod error;
mod token;
mod tokenizer;
mod parser;

use crate::error::{Error};
use crate::tokenizer::tokenize;
use crate::parser::parse;
use create::lib::{read_file};

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        ExitCode::FAILURE
    }

    let command = &args[1];
    let filename = &args[2];

    let file_contents = read_file(filename);

    match command.as_str() {
        "tokenize" => tokenize(&file_contents),
        "parse" => parse(&file_contents),
    }

    ExitCode::SUCCESS
}
