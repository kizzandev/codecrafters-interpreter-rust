use std::process::ExitCode;

use crate::lexer::{Lexer, Token};

fn parse_number(n_raw: &str) -> String {
    if n_raw.contains('.') {
        format!("{n_raw}")
    } else {
        format!("{n_raw}.0")
    }
}

/*fn recursive_parse(lexer: &mut Lexer, depth: usize) -> Result<String, ExitCode> {
    let mut result = String::new();
    let mut has_content = false;
    let mut is_single_depth = false;

    while let Some((t, _line)) = lexer.next() {
        match t {
            Token::ReservedKeyword(k) => {
                has_content = true;
                result.push_str(k)
            }
            Token::Number((n_raw, _n)) => {
                has_content = true;
                result.push_str(&parse_number(n_raw));
            }
            Token::StringLiteral(s) => {
                has_content = true;
                result.push_str(s)
            }
            Token::Character('(') => {
                has_content = true;
                result.push_str(&format!("(group {}", recursive_parse(lexer, depth + 1)?))
            }
            Token::Character(')') => {
                if depth == 0 {
                    eprintln!("IN: CHAR |)|\nOUT: {result}");
                    eprintln!("Error: Unmatched parentheses.");
                    return Err(ExitCode::from(65));
                }
                if !has_content {
                    eprintln!("Error: Empty parentheses.");
                    return Err(ExitCode::from(65));
                }
                return Ok(result + ")");
            }
            Token::Character('!') => {
                has_content = true;
                is_single_depth = true;
                result.push_str(&format!("(! {})", recursive_parse(lexer, depth)?))
            }
            Token::Character('-') => {
                has_content = true;
                is_single_depth = true;
                result.push_str(&format!("(- {})", recursive_parse(lexer, depth)?))
            }
            Token::Character(c) if matches!(c, '*' | '/') => {
                has_content = true;
                is_single_depth = true;
                let right = lexer.next().unwrap().0;
                match right {
                    Token::Number((n_raw, _)) => {
                        let right = parse_number(&n_raw);
                        result = format!("({c} {result} {right})");
                    },
                    Token::Character('(') => {
                        result = format!("(group {c} {result} {}", recursive_parse(lexer, depth + 1)?);
                    },
                    Token::Character('-') => {
                        let right = lexer.next().unwrap().0;
                        let right = match right {
                            Token::Number((n_raw, _)) => parse_number(&n_raw),
                            _ => todo!(),
                        };
                        result = format!("({c} {result} (- {right}))");
                    }
                    _ => continue,
                }

                while let Some((Token::Character(next_op), _)) = lexer.peek() {
                    if matches!(next_op, '*' | '/') {
                        let _ = lexer.next();
                        let next_right = lexer.next().unwrap().0;
                        match next_right {
                            Token::Number((n_raw, _)) => {
                                let right = parse_number(&n_raw);
                                result = format!("({next_op} {result} {right})");
                            },
                            Token::Character('(') => {
                                result = format!("({next_op} {result} (group {})", recursive_parse(lexer, depth + 1)?);
                            },
                            Token::Character('-') => {
                                let right = lexer.next().unwrap().0;
                                let right = match right {
                                    Token::Number((n_raw, _)) => parse_number(&n_raw),
                                    _ => todo!(),
                                };
                                result = format!("({next_op} {result} (- {right}))");
                            },
                            _ => continue,
                        }
                    } else {
                        break;
                    }
                }
            }
            _ => todo!(),
        }
    }

    if depth > 0 && !is_single_depth {
        eprintln!("Error: Unmatched parentheses.");
        eprintln!("Depth: {depth}\n{result}");
        return Err(ExitCode::from(65));
    }
    Ok(result)
}*/

fn parse_primary(lexer: &mut Lexer, depth: usize) -> Result<String, ExitCode> {
    if let Some((t, _)) = lexer.peek() {
        match t {
            Token::ReservedKeyword(k) => Ok(k.to_string()),
            Token::Identifier(i) => Ok(i.to_string()),
            Token::Number((n_raw, _)) => Ok(parse_number(&n_raw)),
            Token::StringLiteral(s) => Ok(s.to_string()),
            Token::Character('(') => {
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
        }
    }
    Ok(result)
}

fn parse_expression(lexer: &mut Lexer, depth: usize) -> Result<String, ExitCode> {
    let mut result = parse_term(lexer, depth)?;
    while let Some((t, _)) = lexer.peek() {
        match t {
            Token::Character(c) if matches!(c, '+' | '-') => {
                let op = lexer.next().unwrap().0;
                let right = parse_term(lexer, depth)?;
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