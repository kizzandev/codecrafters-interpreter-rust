use std::process::ExitCode;

use crate::ast::Expr;
use crate::lexer::{Lexer, Token};

fn parse_number(n_raw: &str) -> String {
    if n_raw.contains('.') {
        let n_raw = n_raw.trim_end_matches('0').trim_end_matches('.');
        if n_raw.contains('.') {
            n_raw.to_string()
        } else {
            format!("{n_raw}.0")
        }
    } else {
        format!("{n_raw}.0")
    }
}

fn parse_primary(lexer: &mut Lexer, depth: usize) -> Result<Expr, ExitCode> {
    if let Some((t, _)) = lexer.next() {
        match t {
            Token::ReservedKeyword(k) => Ok(Expr::ReservedKeyword(k.to_string())),
            Token::Number((n_raw, _)) => Ok(Expr::Number(n_raw.parse().unwrap())),
            Token::StringLiteral(s) => Ok(Expr::StringLiteral(s.to_string())),
            Token::UnterminatedStringLiteral => {
                eprintln!("Error: Unterminated string literal.");
                Err(ExitCode::from(65))
            }
            Token::Character('(') => {
                let expr = match parse_expression(lexer, depth + 1) {
                    Ok(expr) => expr,
                    _ => return Err(ExitCode::from(65)),
                };
                if let Some((Token::Character(')'), _)) = lexer.next() {
                    // Ok(format!("(group {expr})"))
                    Ok(Expr::Grouping(Box::new(expr)))
                } else {
                    eprintln!("Error: Unmatched parentheses.");
                    Err(ExitCode::from(65))
                }
            }
            _ => {
                eprintln!("Error: Unexpected token.");
                Err(ExitCode::from(65))
            }
        }
    } else {
        eprintln!("Error: Unexpected end of input.");
        Err(ExitCode::from(65))
    }
}

fn parse_unary(lexer: &mut Lexer, depth: usize) -> Result<Expr, ExitCode> {
    if let Some((t, _)) = lexer.peek() {
        match t {
            Token::Character('!') | Token::Character('-') => {
                let op = lexer.next().unwrap().0;
                let operand = match parse_unary(lexer, depth) {
                    Ok(operand) => operand,
                    _ => return Err(ExitCode::from(65)),
                };
                Ok(Expr::Unary {
                    op: op.to_string().chars().next().unwrap(),
                    right: Box::new(operand),
                })
            }
            /*Token::Character('!') => {
                lexer.next();
                let operand = match parse_unary(lexer, depth) {
                    Ok(operand) => operand,
                    _ => return Err(ExitCode::from(65)),
                };
                Ok(format!("(! {operand})"))
            }
            Token::Character('-') => {
                lexer.next();
                let operand = match parse_unary(lexer, depth) {
                    Ok(operand) => operand,
                    _ => return Err(ExitCode::from(65)),
                };
                Ok(format!("(- {operand})"))
            }*/
            _ => parse_primary(lexer, depth),
        }
    } else {
        eprintln!("Error: Unexpected end of input.");
        Err(ExitCode::from(65))
    }
}

fn parse_factor(lexer: &mut Lexer, depth: usize) -> Result<Expr, ExitCode> {
    let mut result = match parse_unary(lexer, depth) {
        Ok(s) => s,
        Err(_) => return Err(ExitCode::from(65)),
    };
    while let Some((t, _)) = lexer.peek() {
        match t {
            Token::Character(c) if matches!(c, '*' | '/') => {
                let op = lexer.next().unwrap().0;
                let right = match parse_unary(lexer, depth) {
                    Ok(s) => s,
                    Err(_) => return Err(ExitCode::from(65)),
                };
                // result = format!("({op} {result} {right})");
                result = Expr::Binary {
                    left: Box::new(result),
                    op: op.to_string().chars().next().unwrap(),
                    right: Box::new(right),
                }
            }
            _ => break,
        }
    }
    Ok(result)
}

fn parse_term(lexer: &mut Lexer, depth: usize) -> Result<Expr, ExitCode> {
    let mut result = match parse_factor(lexer, depth) {
        Ok(s) => s,
        Err(_) => return Err(ExitCode::from(65)),
    };
    while let Some((t, _)) = lexer.peek() {
        match t {
            Token::Character(c) if matches!(c, '+' | '-') => {
                let op = lexer.next().unwrap().0;
                let right = match parse_factor(lexer, depth) {
                    Ok(s) => s,
                    Err(_) => return Err(ExitCode::from(65)),
                };
                // result = format!("({op} {result} {right})");
                result = Expr::Binary {
                    left: Box::new(result),
                    op: op.to_string().chars().next().unwrap(),
                    right: Box::new(right),
                }
            }
            _ => break,
        }
    }
    Ok(result)
}

fn parse_comparisson(lexer: &mut Lexer, depth: usize) -> Result<Expr, ExitCode> {
    let mut result = match parse_term(lexer, depth) {
        Ok(s) => s,
        Err(_) => return Err(ExitCode::from(65)),
    };
    while let Some((t, _)) = lexer.peek() {
        match t {
            Token::Character(c) if matches!(c, '<' | '>') => {
                let op = lexer.next().unwrap().0;
                let right = match parse_term(lexer, depth) {
                    Ok(s) => s,
                    Err(_) => return Err(ExitCode::from(65)),
                };
                // result = format!("({op} {result} {right})");
                result = Expr::Binary {
                    left: Box::new(result),
                    op: op.to_string().chars().next().unwrap(),
                    right: Box::new(right),
                }
            }
            Token::CharacterDouble(c1, c2) if matches!(c1, '<' | '>') && c2 == '=' => {
                let op = lexer.next().unwrap().0;
                let right = match parse_term(lexer, depth) {
                    Ok(s) => s,
                    Err(_) => return Err(ExitCode::from(65)),
                };
                // result = format!("({op} {result} {right})");
                result = Expr::Binary {
                    left: Box::new(result),
                    op: op.to_string().chars().next().unwrap(),
                    right: Box::new(right),
                }
            }
            _ => break,
        }
    }
    Ok(result)
}

fn parse_equality(lexer: &mut Lexer, depth: usize) -> Result<Expr, ExitCode> {
    let mut result = match parse_comparisson(lexer, depth) {
        Ok(s) => s,
        Err(_) => return Err(ExitCode::from(65)),
    };
    while let Some((t, _)) = lexer.peek() {
        match t {
            Token::CharacterDouble(c1, c2) if matches!(c1, '=' | '!') && c2 == '=' => {
                let op = lexer.next().unwrap().0;
                let right = match parse_comparisson(lexer, depth) {
                    Ok(s) => s,
                    Err(_) => return Err(ExitCode::from(65)),
                };
                // result = format!("({op} {result} {right})");
                result = Expr::Binary {
                    left: Box::new(result),
                    op: op.to_string().chars().next().unwrap(),
                    right: Box::new(right),
                }
            }
            _ => break,
        }
    }
    Ok(result)
}

fn parse_expression(lexer: &mut Lexer, depth: usize) -> Result<Expr, ExitCode> {
    let result = match parse_equality(lexer, depth) {
        Ok(s) => s,
        Err(_) => return Err(ExitCode::from(65)),
    };
    Ok(result)
}

fn recursive_parse(lexer: &mut Lexer, depth: usize) -> Result<Expr, ExitCode> {
    parse_expression(lexer, depth)
}

pub fn parse(file_contents: &str) -> Result<Expr, ExitCode> {
    let mut lexer = Lexer::new(&file_contents);
    match recursive_parse(&mut lexer, 0) {
        Ok(s) => {
            // println!("{}", s.to_string());
            Ok(s)
        }
        _ => Err(ExitCode::from(65)),
    }
}
