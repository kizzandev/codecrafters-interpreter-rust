use std::env;

mod tokenizer;
use crate::tokenizer::tokenizer;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => tokenize(filename).unwrap(),
        _ => eprintln!("Unknown command: {}", command),
    }
}
