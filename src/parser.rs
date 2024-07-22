use std::process::ExitCode;

use crate::lexer::{Lexer, Token};

pub fn parse(file_contents: &str) -> ExitCode {
    let mut lexer = Lexer::new(&file_contents);

    loop {
        let Some((t, _line)) = lexer.next() else { break; };
        match t {
            Token::ReservedKeyword(k) => println!("{k}"),
            Token::Number((n_raw, n)) => {
                // We check the next token without advancing the iterator
                let symbol = match lexer.peek() {
                    Some((t, _)) => t,
                    None => {
                        if !n_raw.contains('.') {
                            println!("{n_raw}.0");
                        } else {
                            println!("{n_raw}");
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
                                println!("({c} {n:?} {n2:?})");
                            },
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                }
            },
            Token::StringLiteral(s) => println!("{s}"),
            Token::Character(paren) if paren == '(' => {
                let mut group_content = String::new();
                let mut depth = 1;

                while let Some((t, _)) = lexer.next() {
                    match t {
                        Token::Character('(') => {
                            depth += 1;
                            group_content.push('(');
                        }
                        Token::Character(')') => {
                            depth -= 1;
                            if depth == 0 { break; }
                            group_content.push(')');
                        }
                        _ => group_content.push_str(&format!("{t.0}")),
                    }
                }
                if depth != 0 {
                    println!("Error: Unmatched parentheses.");
                } else {
                    println!("(group {group_content})");
                }
            }
            _ => todo!(),
        }
    };
    ExitCode::SUCCESS
}