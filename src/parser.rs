use std::process::ExitCode;

use crate::lexer::{Lexer, Token};

fn parse_number(n_raw: &str) -> String {
    if n_raw.contains('.') {
        format!("{n_raw}")
    } else {
        format!("{n_raw}.0")
    }
}

// Parse only the next token
fn parse_next(lexer: &mut Lexer, depth: usize) -> Result<String, ExitCode> {
    let mut result = String::new();
    
    if let Some((t, _line)) = lexer.next() {
        match t {
            Token::ReservedKeyword(k) => result.push_str(k),
            Token::Number((n_raw, _n)) => result.push_str(&parse_number(n_raw)),
            Token::StringLiteral(s) => result.push_str(s),
            Token::Character('(') => result.push_str(&format!("(group {}", parse_next(lexer, depth + 1)?)),
            Token::Character(')') => {
                if depth == 0 { return Err(ExitCode::from(65)); }
                result.push_str(")");
            },
            Token::Character('!') => result.push_str(&format!("(! {})", parse_next(lexer, depth)?)),
            Token::Character('-') => result.push_str(&format!("(- {})", parse_next(lexer, depth)?)),
            Token::Character(c) if matches!(c, '*' | '/') => {
                result.push_str(&format!("({c} {})", parse_next(lexer, depth)?));
            },
            _ => return Err(ExitCode::from(65)),
        }
    } else {
        return Err(ExitCode::from(65));
    }
    Ok(result)
}

// To be seen if used
/*fn _recursive_parse(lexer: &mut Lexer, depth: usize) -> Result<String, ExitCode> {
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
                        result = format!("(group {c} {result} {})", recursive_parse(lexer, depth + 1)?);
                    },
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

pub fn parse(file_contents: &str) -> ExitCode {
    let mut lexer = Lexer::new(&file_contents);

    // Using parse_next
    match parse_next(&mut lexer, 0) {
        Ok(s) => {
            println!("{s}");
            ExitCode::SUCCESS
        },
        _ => ExitCode::from(65),
    }


    /*match recursive_parse(&mut lexer, 0) {
        Ok(s) => {
            println!("{s}");
            ExitCode::SUCCESS
        },
        _ => ExitCode::from(65),
    }*/
}