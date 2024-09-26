use std::process::ExitCode;

use crate::ast::Expr;
use crate::evaluator::evaluate;
use crate::lexer::{Lexer, Token};

fn _parse_number(n_raw: &str) -> String {
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
            Token::ReservedKeyword(k) => match k {
                "print" => {
                    let expr = match parse_expression(lexer, depth) {
                        Ok(expr) => {
                            let expr_type = expr.get_type();
                            match expr_type.as_str() {
                                "Unary" => {
                                    let eval_expr = evaluate(&expr);
                                    match eval_expr.to_string().as_str() {
                                        boolean if matches!(boolean, "true" | "false") => {
                                            // eprintln!("boolean: {}", boolean);
                                            Expr::ReservedKeyword(
                                                "print".to_string() + &boolean.to_string(),
                                            )
                                        }
                                        _ => {
                                            eprintln!("eval_expr: {}", eval_expr.to_string());
                                            expr
                                        }
                                    }
                                }
                                _ => expr,
                            }
                            // eprintln!("expr type: {}", expr.get_type());
                            // let eval_expr = evaluate(&expr);
                            // eprintln!("eval_expr: {}", eval_expr.to_string());
                            // let new_str = "print".to_string() + &expr.to_string();
                            // let eval_expr_str = eval_expr.to_string();
                            // eprintln!("PRINT: {}", eval_expr_str);
                            // let new_expr = expr.change_value(eval_expr_str).clone();
                            // let new_str = "print".to_string() + &new_expr.to_string();
                            // let temp = expr.change_value(new_str).clone();
                            // eprintln!("new expr: {}", temp.to_string());
                            // temp
                            // expr
                        }
                        _ => return Err(ExitCode::from(65)),
                    };
                    if let Some((Token::Character(';'), _)) = lexer.next() {
                        let expr = expr.clone();
                        Ok(expr)
                        /*Ok(Expr::ReservedKeyword(
                            "print".to_string() + &expr.to_string(),
                        ))*/
                    } else {
                        eprintln!("Error: Expected ';' after 'print'.");
                        return Err(ExitCode::from(65));
                    }
                }
                _ => Ok(Expr::ReservedKeyword(k.to_string())),
            },
            Token::Number((_, n)) => Ok(Expr::Number(n)),
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
            Token::Character(';') => Ok(Expr::Character(';')),
            _ => {
                eprintln!(
                    "Error: Unexpected token at {}:{}.",
                    lexer.get_line(),
                    lexer.get_index()
                );
                Err(ExitCode::from(65))
            }
        }
    } else {
        // eprintln!("Error: Unexpected end of input.");
        // eprintln!("The end?");
        // Err(ExitCode::from(65))
        Ok(Expr::EOF)
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
        // eprintln!("Error: Unexpected end of input.");
        // Err(ExitCode::from(65))
        parse_primary(lexer, depth)
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
                result = Expr::Comparison {
                    left: Box::new(result),
                    op: match op.to_string().chars().next().unwrap() {
                        '<' => ('<', '='),
                        '>' => ('>', '='),
                        _ => unreachable!(),
                    },
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
                result = Expr::Comparison {
                    left: Box::new(result),
                    op: match op.to_string().chars().next().unwrap() {
                        '=' => ('=', '='),
                        '!' => ('!', '='),
                        _ => unreachable!(),
                    },
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

#[derive(PartialEq)]
pub enum ParseOption {
    RUN,
    EVALUATE,
    PARSE,
    _TOKENIZE,
}

pub fn parse(file_contents: &str, option: ParseOption) -> Result<Vec<Expr>, ExitCode> {
    let mut lexer = Lexer::new(&file_contents);

    let mut results: Vec<Expr> = Vec::new();

    loop {
        match recursive_parse(&mut lexer, 0) {
            Ok(expr) if expr.to_string() == "EOF" => break,
            /*Ok(expr) if expr.to_string() == ";" => {
                eprintln!(";");
                continue;
            }*/
            Ok(expr) if option == ParseOption::EVALUATE => {
                if expr.to_string() == ";" {
                    continue;
                };
                results.push(expr)
            }
            Ok(mut expr) if option == ParseOption::RUN => {
                // eprintln!("EXPR: {}", expr.to_string());
                if expr.to_string().starts_with("print") {
                    // let new_expr =
                    //     Expr::ReservedKeyword(expr.to_string().split_at(5).1.to_string());
                    let new_expr = expr
                        .change_value(expr.to_string().split_at(5).1.to_string())
                        .clone();
                    // eprintln!("{}", new_expr.to_string());
                    results.push(new_expr);
                };
            }
            _ => return Err(ExitCode::from(65)),
        }
    }

    Ok(results)

    /*match recursive_parse(&mut lexer, 0) {
        Ok(s) => {
            // println!("{}", s.to_string());
            Ok(s)
        }
        _ => Err(ExitCode::from(65)),
    }*/
}
