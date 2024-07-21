use std::process::ExitCode;

use crate::lexer::{Lexer, Token};

pub fn parse(file_contents: &str) -> ExitCode {
    let mut lexer = Lexer::new(&file_contents);

    loop {
        eprintln!("Parse LOOP");
        let Some((t, _line)) = lexer.next() else { break; };
        eprintln!("Start to match the token...");
        match t {
            Token::ReservedKeyword(k) => {
                eprintln!("TOKEN: ReservedKeyword");
                println!("{k}")
            },
            Token::Number((_, n)) => {
                eprintln!("TOKEN: Number");
                // We check the next token without advancing the iterator
                let symbol = match lexer.peek() {
                    Some((t, _)) => t,
                    None => break,
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
                            _ => eprintln!("NaN"),
                        }
                    }
                    _ => eprintln!("Another SYMBOL"),
                }
            },
            _ => eprintln!("TOKEN: <something_else>"),
        }
    };
    ExitCode::SUCCESS
}