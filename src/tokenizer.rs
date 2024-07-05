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
    
    let mut line = 1usize;
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
            '/' => {
                let mut peekable = chars.clone().peekable();
                if peekable.next() == Some('/') {
                    while let Some(c) = chars.next() {
                        if c == '\n' {
                            line += 1;
                            break;
                        }
                    }
                } else {
                    tokens.push(Token::new(TokenType::SLASH, c.to_string()));
                }
            },
            '=' => {
                let mut peekable = chars.clone().peekable();
                if peekable.next() == Some('=') {
                    tokens.push(Token::new(TokenType::EQUAL_EQUAL, "==".to_string()));
                    chars.next();
                } else {
                    tokens.push(Token::new(TokenType::EQUAL, c.to_string()));
                }
            },
            '!' => {
                let mut peekable = chars.clone().peekable();
                if peekable.next() == Some('=') {
                    tokens.push(Token::new(TokenType::BANG_EQUAL, "!=".to_string()));
                    chars.next();
                } else {
                    tokens.push(Token::new(TokenType::BANG, c.to_string()));
                }
            },
            '<' => {
                let mut peekable = chars.clone().peekable();
                if peekable.next() == Some('=') {
                    tokens.push(Token::new(TokenType::LESS_EQUAL, "<=".to_string()));
                    chars.next();
                } else {
                    tokens.push(Token::new(TokenType::LESS, c.to_string()));
                }
            },
            '>' => {
                let mut peekable = chars.clone().peekable();
                if peekable.next() == Some('=') {
                    tokens.push(Token::new(TokenType::GREATER_EQUAL, ">=".to_string()));
                    chars.next();
                } else {
                    tokens.push(Token::new(TokenType::GREATER, c.to_string()));
                }
            },
            ' ' | '\t' | '\r' => continue,
            '\n' => line += 1,
            '"' => {
                let mut string = String::new();
                string.push(c);
                while let Some(c) = chars.next() {
                    if c == '"' {
                        string.push(c);
                        tokens.push(Token::new_with_value(
                            TokenType::STRING,
                            string.clone(),
                            string.clone()[1..string.len()-1].to_string()));
                        break;
                    } else {
                        string.push(c);
                    }
                }

                if string.chars().last() != Some('"') {
                    eprintln!("[line {}] Error: Unterminated string.", line);
                    has_error = true
                }
            },
            c if c.is_ascii_digit() => {
                let mut number = String::new();
                number.push(c);
                let mut has_dot = false;
                while let Some(c) = chars.next() {
                    if c == '.' && !has_dot || c.is_ascii_digit() {
                        if c == '.' {
                            let mut peekable = chars.clone().peekable();
                            let Some(c) = peekable.next() else { break; };
                            if !c.is_ascii_digit() {
                                break;
                            }
                            has_dot = true;
                        }
                        number.push(c);
                    } else {
                        break;
                    }
                }

                tokens.push(Token::new_with_value(TokenType::NUMBER, number.clone(), number.to_string()));
            }
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