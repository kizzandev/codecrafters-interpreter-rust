use std::process::ExitCode;

use crate::lexer::{Lexer, Token};

fn recursive_parse(lexer: &mut Lexer, depth: usize) -> Result<String, ExitCode> {
    let mut result = String::new();
    let mut has_content = false;
    let mut is_single_depth = false;

    while let Some((t, _line)) = lexer.next() {
        eprintln!("Result: {result}");
        match t {
            Token::ReservedKeyword(k) => {
                has_content = true;
                result.push_str(k)
            }
            Token::Number((n_raw, n)) => {
                // We check the next token without advancing the iterator
                let symbol = match lexer.peek() {
                    Some((t, _)) => t,
                    None => {
                        has_content = true;
                        if !n_raw.contains('.') {
                            result.push_str(&format!("{n_raw}.0"));
                        } else {
                            result.push_str(n_raw);
                        }
                        continue;
                    },
                };
                match symbol {
                    Token::Character(c) if matches!(c, '+' | '-' | '*' | '/') => {
                        // It's a binary operation
                        // Next-ed twice because the peek() clones the iterator
                        lexer.next();
                        let n2 = lexer.next().unwrap().0;
                        match n2 {
                            Token::Number((_, n2)) => {
                                result.push_str(&format!("{c} {n:?} {n2:?}"));
                            },
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                }
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
                let op = match t {
                    Token::Character('*') => "*",
                    Token::Character('/') => "/",
                    _ => unreachable!(),
                };
                let left = result;
                eprintln!("Left: {left}");
                let right = recursive_parse(lexer, depth)?;
                result = format!("({op} {left} {right})");

                while let Some((Token::Character(next_op), _)) = lexer.peek() {
                    if matches!(next_op, '*' | '/') {
                        let op = match next_op {
                            '*' => "*",
                            '/' => "/",
                            _ => unreachable!(),
                        };
                        let _ = lexer.next();
                        let right = recursive_parse(lexer, depth)?;
                        result = format!("({op} {left} {right})");
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