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
                            string.clone()[1..string.len()-1]));
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
                let mut peekable = chars.clone().peekable();
                while let Some(next_c) = peekable.next() {
                    if next_c.is_ascii_digit() {
                        number.push(next_c);
                        chars.next();
                    } else {
                        if next_c == '.' && !has_dot {
                            number.push(next_c);
                            chars.next();
                            has_dot = true;
                        } else {
                            break;
                        }
                    }
                }

                if let Some('.') = number.chars().last() {
                    let value = number.clone()[0..number.len()-1].to_string();
                    number.push('0');
                    tokens.push(Token::new_with_value(TokenType::NUMBER, value, number));
                    tokens.push(Token::new(TokenType::DOT, ".".to_string()));
                } else {
                    if !has_dot {
                        let value = number.clone();
                        number.push('.');
                        number.push('0');
                        tokens.push(Token::new_with_value(TokenType::NUMBER, value, number));
                    } else {
                        let value = number.clone();
                        while number.chars().last() == Some('0') && number.chars().nth(number.len()-2) == Some('0') {
                            number.pop();
                        }
                        tokens.push(Token::new_with_value(TokenType::NUMBER, value, number));
                    }
                }
            }
            c if c.is_alphabetic() || c == '_' => {
                let mut identifier = String::new();
                identifier.push(c);
                let mut peekable = chars.clone().peekable();
                while let Some(next_c) = peekable.next() {
                    if next_c.is_alphanumeric() || next_c == '_' {
                        identifier.push(next_c);
                        chars.next();
                    } else {
                        break;
                    }
                }

                match identifier.as_str() {
                    "and" => tokens.push(Token::new(TokenType::AND, identifier)),
                    "class" => tokens.push(Token::new(TokenType::CLASS, identifier)),
                    "else" => tokens.push(Token::new(TokenType::ELSE, identifier)),
                    "false" => tokens.push(Token::new(TokenType::FALSE, identifier)),
                    "for" => tokens.push(Token::new(TokenType::FOR, identifier)),
                    "fun" => tokens.push(Token::new(TokenType::FUN, identifier)),
                    "if" => tokens.push(Token::new(TokenType::IF, identifier)),
                    "nil" => tokens.push(Token::new(TokenType::NIL, identifier)),
                    "or" => tokens.push(Token::new(TokenType::OR, identifier)),
                    "print" => tokens.push(Token::new(TokenType::PRINT, identifier)),
                    "return" => tokens.push(Token::new(TokenType::RETURN, identifier)),
                    "super" => tokens.push(Token::new(TokenType::SUPER, identifier)),
                    "this" => tokens.push(Token::new(TokenType::THIS, identifier)),
                    "true" => tokens.push(Token::new(TokenType::TRUE, identifier)),
                    "var" => tokens.push(Token::new(TokenType::VAR, identifier)),
                    "while" => tokens.push(Token::new(TokenType::WHILE, identifier)),
                    _ => tokens.push(Token::new(TokenType::IDENTIFIER, identifier)),
                }
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