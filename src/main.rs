use std::env;
use std::process::exit;

mod error;
mod token;
mod tokenizer;

use crate::error::{Error};
use crate::tokenizer::tokenize;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => match tokenize(filename) {
            Err(e) => {
                let e: Error = e.downcast().unwrap();
                exit(e.exit_code as i32);
            }
            _ => {}
        },
        _ => eprintln!("Unknown command: {}", command),
    }
}
