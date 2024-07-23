use std::process::ExitCode;

use crate::lexer::{Lexer, Token};

fn parse_number(n_raw: &str) -> String {
    if n_raw.contains('.') {
        format!("{n_raw}")
    } else {
        format!("{n_raw}.0")
    }
}

fn parse_primary(lexer: &mut Lexer, depth: usize) -> Result<String, ExitCode> {
    if let Some((t, _)) = lexer.peek() {
        match t {
            Token::ReservedKeyword(k) => {
                lexer.next();
                Ok(k.to_string())
            },
            Token::Identifier(i) => {
                lexer.next();
                Ok(i.to_string())
            },
            Token::Number((n_raw, _)) => {
                lexer.next();
                Ok(parse_number(&n_raw))
            },
            Token::StringLiteral(s) => {
                lexer.next();
                Ok(s.to_string())
            },
            Token::Character('(') => {
                lexer.next();
                let expr = parse_expression(lexer, depth + 1)?;
                if let Some((Token::Character(')'), _)) = lexer.next() {
                    Ok(format!("(group {expr})"))
                } else {
                    eprintln!("Error: Unmatched parentheses.");
                    Err(ExitCode::from(65))
                }
            }
            _ => {
                eprintln!("Error: Unexpected token.");
                Err(ExitCode::from(65))
            },
        }
    } else {
        eprintln!("Error: Unexpected end of input.");
        Err(ExitCode::from(65))
    }
}

fn parse_unary(lexer: &mut Lexer, depth: usize) -> Result<String, ExitCode> {
    if let Some((t, _)) = lexer.peek() {
        match t {
            Token::Character('!') => {
                lexer.next();
                let operand = parse_unary(lexer, depth)?;
                Ok(format!("(! {operand})"))
            }
            Token::Character('-') => {
                lexer.next();
                let operand = parse_unary(lexer, depth)?;
                Ok(format!("(- {operand})"))
            }
            _ => parse_primary(lexer, depth),
        }
    } else {
        eprintln!("Error: Unexpected end of input.");
        Err(ExitCode::from(65))
    }
}

fn parse_term(lexer: &mut Lexer, depth: usize) -> Result<String, ExitCode> {
    let mut result = parse_unary(lexer, depth)?;
    while let Some((t, _)) = lexer.peek() {
        match t {
            Token::Character(c) if matches!(c, '*' | '/') => {
                let op = lexer.next().unwrap().0;
                let right = parse_unary(lexer, depth)?;
                result = format!("({op} {result} {right})");
            }
            _ => break,
        }
    }
    Ok(result)
}

fn parse_comparisson(lexer: &mut Lexer, depth: usize) -> Result<String, ExitCode> {
    let mut result = parse_term(lexer, depth)?;
    while let Some((t, _)) = lexer.peek() {
        match t {
            Token::Character(c) if matches!(c, '<' | '>') => {
                let op = lexer.next().unwrap().0;
                let right = parse_term(lexer, depth)?;
                result = format!("({op} {result} {right})");
            }
            Token::CharacterDouble(c1, c2) if matches!(c1, '=' | '!') && c2 == '=' => {
                let op = lexer.next().unwrap().0;
                let right = parse_term(lexer, depth)?;
                result = format!("({op} {result} {right})");
            }
            _ => break,
        }
    }
    Ok(result)
}

fn parse_expression(lexer: &mut Lexer, depth: usize) -> Result<String, ExitCode> {
    let mut result = parse_comparisson(lexer, depth)?;
    while let Some((t, _)) = lexer.peek() {
        match t {
            Token::Character(c) if matches!(c, '+' | '-') => {
                let op = lexer.next().unwrap().0;
                let right = parse_comparisson(lexer, depth)?;
                result = format!("({op} {result} {right})");
            }
            _ => break,
        }
    }
    Ok(result)
}

fn recursive_parse(lexer: &mut Lexer, depth: usize) -> Result<String, ExitCode> {
    parse_expression(lexer, depth)
}

pub fn parse(file_contents: &str) -> ExitCode {
    let mut lexer = Lexer::new(&file_contents);
    match recursive_parse(&mut lexer, 0) {
        Ok(s) => {
            println!("{s}");
            ExitCode::SUCCESS
        },
        _ => ExitCode::from(65),
    }
}