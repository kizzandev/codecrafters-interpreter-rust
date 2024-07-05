use std::fmt::Display;

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum TokenType {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,

    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    STAR,
    SLASH,

    EQUAL,
    EQUAL_EQUAL,
    BANG,
    BANG_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    STRING,
    NUMBER,

    IDENTIFIER,

    EOF,
}

pub struct Token {
    _type: TokenType,
    _string: String,
    _value: Option<String>,
}

impl Token {
    pub fn new(_type: TokenType, _string: String) -> Self {
        Token {
            _type,
            _string,
            _value: None,
        }
    }

    pub fn new_with_value(_type: TokenType, _string: String, _value: String) -> Self {
        Token {
            _type,
            _string,
            _value: Some(_value),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {} {}", self._type, self._string, self._value.clone().unwrap_or("null".to_string()))
    }
}