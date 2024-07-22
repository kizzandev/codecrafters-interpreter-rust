use std::process::ExitCode;

use crate::lexer::{Lexer, Token};

fn recursive_parse(lexer: &mut Lexer, depth: usize) -> Result<String, ExitCode> {
    let mut result = String::new();
    let mut has_content = false;

    while let Some((t, _line)) = lexer.next() {
        match t {
            Token::ReservedKeyword(k) => {
                has_content = true;
                result.push_str(k)
            },
            Token::Number((n_raw, n)) => {
                // We check the next token without advancing the iterator
                let symbol = match lexer.peek() {
                    Some((t, _)) => t,
                    None => {
                        if !n_raw.contains('.') {
                            result.push_str(&format!("{n_raw}.0"));
                        } else {
                            result.push_str(n_raw);
                        }
                        has_content = true;
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
            },
            Token::StringLiteral(s) => {
                has_content = true;
                result.push_str(s)
            },
            Token::Character('(') => {
                result.push_str(&format!("(group {}", recursive_parse(lexer, depth + 1)?));
            }
            Token::Character(')') => {
                if depth == 0 {
                    eprintln!("Error: Unmatched parentheses.");
                    return Err(ExitCode::FAILURE);
                }
                if !has_content {
                    return Err(ExitCode::FAILURE);
                }
                return Ok(result + ")");
            }
            _ => todo!(),
        }
    }
    
    if depth > 0 {
        eprintln!("Error: Unmatched parentheses.");
        return Err(ExitCode::FAILURE);
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
        _ => ExitCode::FAILURE,
    }
}