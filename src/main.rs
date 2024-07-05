use std::env;
use std::fs;

use anyhow::bail;

mod token;
use crate::token::{Token, TokenType};

fn tokenize(filename: &String) -> anyhow::Result<()> {
    let file_contents = fs::read_to_string(filename)?;
    let mut chars = file_contents.chars();

    let mut tokens = vec![];

    while let Some(c) = chars.next() {
        match c {
            '(' => tokens.push(Token::new(TokenType::LEFT_PAREN, c.to_string())),
            ')' => tokens.push(Token::new(TokenType::RIGHT_PAREN, c.to_string())),
            _ => bail!("Unknown character: {}", c),
        }
    }

    tokens.push(Token::new(TokenType::EOF, "".to_string()));

    for token in tokens {
        println!("{}", token);
    }

    Ok(())
}

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
