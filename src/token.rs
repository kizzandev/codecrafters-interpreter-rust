use std::fmt::Display;

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum TokenType {
    LEFT_PAREN,
    RIGHT_PAREN,

    EOF,
}

pub struct Token {
    _type: TokenType,
    _string: String,
    _value: Option<String>,
}

pub impl Token {
    fn new(_type: TokenType, _string: String) -> Self {
        Token {
            _type,
            _string,
            _value: None,
        }
    }
}

pub impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {} {}", self._type, self._string, self._value.clone().unwrap_or("null".to_string()))
    }
}