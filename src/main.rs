use std::env;
use std::fs;
use std::fmt::Display;

enum Token {
    LeftParen,
    RightParen,

    Unknown,
    Eof,
    Total_tokens,
}

impl From<char> for Token {
    fn from(c: char) -> Self {
        match c {
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            _ => Token::Unknown,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Token::LeftParen => "LEFT_PAREN",
            Token::RightParen => "RIGHT_PAREN",
            Token::Unknown => "UNKNOWN",
            Token::Eof => "EOF",
        };
        f.write_str(s)
    }
}

struct TokenResult(Token, String);

impl From<char> for TokenResult {
    fn from(c: char) -> Self {
        Self(Token::from(c), c.to_string())
    }
}

impl Display for TokenResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{} {}", self.0, self.1))
    }
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
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            if !file_contents.is_empty() {
                for char in file_contents.chars() {
                    if char == '\n' {
                        continue;
                    }
                    let result = TokenResult::from(char);
                    println!("{}", result);
                }
                println!("EOF  null");
            } else {
                println!("EOF  null"); // Placeholder, remove this line when implementing the scanner
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}
