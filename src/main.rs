use std::env;
use std::fs;
use std::fmt::Display;

use anyhow::bail;

#[derive(Debug)]
#[allow(non_camel_case_types)]
enum TokenType {
    LEFT_PAREN,
    RIGHT_PAREN,

    EOF,
}

struct Token {
    _type: TokenType,
    _string: String,
    _value: Option<String>,
}

impl Token {
    fn new(_type: TokenType, _string: String) -> Self {
        Token {
            _type,
            _string,
            _value: None,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        eprintln!("{:?} {} {}", self._type, self._string, self._value.clone().unwrap_or("null".to_string()))
    }
}

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
        eprintln!("{}", token);
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
