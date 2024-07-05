mod token;
use crate::token::{Token, TokenType};
use anyhow::bail;

pub fn tokenize(filename: &String) -> anyhow::Result<()> {
    let file_contents = fs::read_to_string(filename)?;
    let mut chars = file_contents.chars();

    let mut tokens = vec![];

    while let Some(c) = chars.next() {
        match c {
            '(' => tokens.push(Token::new(TokenType::LEFT_PAREN, c.to_string())),
            ')' => tokens.push(Token::new(TokenType::RIGHT_PAREN, c.to_string())),
            _ => bail!("Unknown character: {}", c),
        }
    }

    tokens.push(Token::new(TokenType::EOF, "".to_string()));

    for token in tokens {
        println!("{}", token);
    }

    Ok(())
}