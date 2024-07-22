use std::process::ExitCode;

use crate::lexer::{Lexer, Token};

pub fn parse(file_contents: &str) -> ExitCode {
    let mut lexer = Lexer::new(&file_contents);

    loop {
        let Some((t, _line)) = lexer.next() else { break; };
        match t {
            Token::ReservedKeyword(k) => {
                println!("{k}")
            },
            Token::Number((n_raw, n)) => {
                // We check the next token without advancing the iterator
                let symbol = match lexer.peek() {
                    Some((t, _)) => t,
                    None => {
                        println!("{n_raw}");
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
            _ => todo!(),
        }
    };
    ExitCode::SUCCESS
}