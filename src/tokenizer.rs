use std::process::ExitCode;

use crate::lexer::{Lexer, Token};

pub fn tokenize(file_contents: &str) -> ExitCode {
    let lexer = Lexer::new(&file_contents);
    let mut success = true;

    for (token, line) in lexer {
        match token {
            Token::ReservedKeyword(k) => println!("{} {} null", k.to_uppercase(), k),
            Token::Identifier(i) => println!("IDENTIFIER {} null", i),
            Token::StringLiteral(s) => println!("STRING \"{s}\" {s}"),
            Token::UnterminatedStringLiteral => {
                success = false;
                println!("[line {line}] ERROR: Unterminated string literal");
            },
            Token::Number((raw_s, n)) => println!("NUMBER {raw_s} {n:?}"),
            Token::CharacterDouble(c1, c2) => {
                let one_of = match (c1, c2) {
                    ('=', '=') => "EQUAL_EQUAL",
                    ('!', '=') => "BANG_EQUAL",
                    ('<', '=') => "LESS_EQUAL",
                    ('>', '=') => "GREATER_EQUAL",
                    ('&', '&') => "AMPERSAND_AMPERSAND",
                    ('|', '|') => "BAR_BAR",
                    _ => unreachable!(),
                };
                println!("{one_of} {c1}{c2} null")
            },
            Token::Character(c) => {
                let one_of = match c {
                    '(' => "LEFT_PAREN",
                    ')' => "RIGHT_PAREN",
                    '{' => "LEFT_BRACE",
                    '}' => "RIGHT_BRACE",
                    ',' => "COMMA",
                    '.' => "DOT",
                    '-' => "MINUS",
                    '+' => "PLUS",
                    ';' => "SEMICOLON",
                    '*' => "STAR",
                    '/' => "SLASH",
                    '=' => "EQUAL",
                    '!' => "BANG",
                    '<' => "LESS",
                    '>' => "GREATER",
                    _ => "UNKNOWN",
                };
                if one_of == "UNKNOWN" {
                    success = false;
                    eprintln!("[line {line}] Error: Unknown character: {c}");
                } else {
                    println!("{one_of} {c} null")
                }
            },
        };
    };
    println!("EOF  null");
    if !success { return ExitCode::from(65); }
    ExitCode::SUCCESS
}
