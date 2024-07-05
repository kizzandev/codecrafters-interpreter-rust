use std::fs;

use crate::token::{Token, TokenType};
use anyhow::bail;

pub fn tokenize(filename: &String) -> anyhow::Result<()> {
    let file_contents = fs::read_to_string(filename)?;
    let mut chars = file_contents.chars();

    let mut tokens = vec![];

    let line = 1usize;

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
            _ => eprint!("[line {}] Error: Unexpected character: {}", line, c),
        }
    }

    tokens.push(Token::new(TokenType::EOF, "".to_string()));

    for token in tokens {
        println!("{}", token);
    }

    Ok(())
}