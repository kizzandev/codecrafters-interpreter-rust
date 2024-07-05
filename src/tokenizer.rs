use std::fs;
use anyhow::bail;

use crate::token::{Token, TokenType};
use crate::error::{Error};

pub fn tokenize(filename: &String) -> anyhow::Result<()> {
    let file_contents = match fs::read_to_string(filename) {
        Ok(contents) => contents,
        Err(_) => bail!(Error::new(u8::MAX))
    };
    let mut chars = file_contents.chars();
    
    let line = 1usize;
    let mut has_error = false;
    
    let mut tokens = vec![];
    while let Some(c) = chars.next() {
        match c {
            '(' => tokens.push(Token::new(TokenType::LEFT_PAREN, c.to_string())),
            ')' => tokens.push(Token::new(TokenType::RIGHT_PAREN, c.to_string())),
            '{' => tokens.push(Token::new(TokenType::LEFT_BRACE, c.to_string())),
            '}' => tokens.push(Token::new(TokenType::RIGHT_BRACE, c.to_string())),
            ',' => tokens.push(Token::new(TokenType::COMMA, c.to_string())),
            '.' => tokens.push(Token::new(TokenType::DOT, c.to_string())),
            '-' => tokens.push(Token::new(TokenType::MINUS, c.to_string())),
            '+' => tokens.push(Token::new(TokenType::PLUS, c.to_string())),
            ';' => tokens.push(Token::new(TokenType::SEMICOLON, c.to_string())),
            '*' => tokens.push(Token::new(TokenType::STAR, c.to_string())),
            '/' => tokens.push(Token::new(TokenType::FOWARD_SLASH, c.to_string())),
            '=' => {
                // peekable
                let mut peekable = chars.clone().peekable();
                if peekable.next() == Some('=') {
                    tokens.push(Token::new(TokenType::EQUAL_EQUAL, "==".to_string()));
                    chars.next();
                } else {
                    tokens.push(Token::new(TokenType::EQUAL, c.to_string()));
                }
            },
            _ => {
                eprintln!("[line {}] Error: Unexpected character: {}", line, c);
                has_error = true
            },
        }
    }

    tokens.push(Token::new(TokenType::EOF, "".to_string()));

    for token in tokens {
        println!("{}", token);
    }

    return if has_error { bail!(Error::new(65)) } else { Ok(()) };
}