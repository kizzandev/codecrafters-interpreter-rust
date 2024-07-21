use std::collections::HashSet;

pub struct Lexer<'input> {
    contents: &'input str,                  // contents of the file
    char_indicies: Vec<(usize, char)>,      // each char and its index
    idx: usize,                             // current index
    line: usize,                            // current line
    is_comment: bool,                       // whether the current token is a comment
    reserved_keywords: HashSet<&'input str> // set of reserved keywords
}

// The keywords to be used
const RESERVED_KEYWORDS: &'static [&'static str] = &[
    "and", "class", "else", "false", "for", "fun", "if", "nil", "or", "print",
    "return", "super", "this", "true", "var", "while"
];

// Two characters that works as a single token
const DOUBLE_CHARACTERS: &'static [(char, char)] = &[
    ('=', '='), ('!', '='), ('<', '='), ('>', '='), ('/', '/')
];

impl<'input> Lexer<'input> {
    pub fn new(contents: &'input str) -> Self {
        let reserved_keywords = RESERVED_KEYWORDS
                                .iter()         // iterates over the array
                                .map(|k| *k)    // converts the array to a vector
                                .collect();     // collects the vector into a HashSet
        Self {
            contents,
            char_indicies: contents
                            .char_indices()        // get the char indicies
                            .collect::<Vec<_>>(),   // convert the char indicies to a vector
            idx: 0,
            line: 1,
            is_comment: false,
            reserved_keywords
        }
    }
}

// Any Token may be one of the following
// Using enum for the Named Tuples
#[derive(Debug)]
pub enum Token<'input> {
    ReservedKeyword(&'input str),
    Identifier(&'input str),
    Number((&'input str, f64)),
    StringLiteral(&'input str),
    Character(char),
    CharacterDouble(char, char),
}

// My current favourite looper thingy (a.k.a. generator)
impl<'input> Iterator for Lexer<'input> {
    type Item = (Token<'input>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx >= self.char_indicies.len() {
            return None;
        }

        // Get the current char and increment the index
        let (c_idx, c) = self.char_indicies[self.idx];
        self.idx += 1;

        // Newline
        if c == '\n' {
            self.line += 1;
            if self.is_comment {
                self.is_comment = false; // reset the comment flag
            }
            return self.next();
        }

        // Comment
        if self.is_comment { return self.next(); }

        // Whitespace
        if c == ' ' || c == '\t' { return self.next(); }

        // Keywords and identifiers
        if c.is_alphabetic() || c == '_' {
            loop {
                // Check if we are at the end
                if self.idx >= self.char_indicies.len() {
                    let identifier = &self.contents[c_idx..]; // extract the identifier
                    if self.reserved_keywords.contains(identifier) {
                        return Some((Token::ReservedKeyword(identifier), self.line));
                    }
                    // Not a reserved keyword
                    return Some((
                        Token::Identifier(identifier),
                        self.line
                    ))
                }

                let (c_next_idx, _c_next) = self.char_indicies[self.idx];
                if !c.is_ascii_alphanumeric() && c != '_' {
                    let id = &self.contents[c_idx..c_next_idx];
                    if self.reserved_keywords.contains(id) {
                        return Some((Token::ReservedKeyword(id), self.line));
                    }
                    return Some((Token::Identifier(id), self.line));
                }
            }
        }

        // Strings
        if c == '"' {
            loop {
                // Check if we are at the end
                if self.idx >= self.char_indicies.len() {
                    return Some((Token::StringLiteral(&self.contents[c_idx..]), self.line));
                }

                let (c_next_idx, c_next) = self.char_indicies[self.idx];
                if c_next == '"' {
                    self.idx += 1;
                    return Some((Token::StringLiteral(&self.contents[c_idx..c_next_idx]), self.line));
                }
            }
        }

        // Numbers
        if c.is_ascii_digit() {
            let mut seen_digit = false;
            loop {
                let (n, n_raw) = if self.idx >= self.char_indicies.len() {
                    // End of string
                    let raw = &self.contents[c_idx..]; // get rest of the string
                    // Possible edge case where the last character is NOT
                    // a valid value
                    let (_, c) = self.char_indicies[self.idx - 1];
                    if c != ' ' && c != '\t' && c != '\n' && !c.is_ascii_digit() {
                        self.idx -= 1;
                    }
                    (raw.parse::<f64>().unwrap(), raw.trim_end_matches("."))
                } else {
                    let (c_next_idx, _c_next) = self.char_indicies[self.idx];
                    let raw = &self.contents[c_idx..c_next_idx];
                    if c.is_ascii_whitespace() {
                        (raw.parse::<f64>().unwrap(), raw)
                    } else if !c.is_ascii_digit() {
                        if c == '.' && !seen_digit {
                            seen_digit = true;
                            continue;
                        };
                        (raw.parse::<f64>().unwrap(), raw)
                    } else { continue; };
                    (raw.parse::<f64>().unwrap(), raw)
                    
                };
                return Some((Token::Number((n_raw, n)), self.line));
            }
        }

        // Double characters
        for (start, next) in DOUBLE_CHARACTERS {
            // self.idx is already incremented
            if c == *start && self.idx < self.char_indicies.len() {
                let (_, c_next) = self.char_indicies[self.idx];
                if c_next == *next {
                    self.idx += 1;
                    // Check if its a comment
                    if *start == '/' && c_next == '/' {
                        self.is_comment = true;
                        return self.next();
                    }
                    return Some((Token::CharacterDouble(*start, *next), self.line));
                }
            }
        }

        // Single characters
        return Some((Token::Character(
                        self.char_indicies[self.idx - 1].1),
                        self.line));
    }
}

// Clone the iterator
impl<'input> Clone for Lexer<'input> {
    fn clone(&self) -> Self {
        Self {
            contents: self.contents,
            char_indicies: self.char_indicies.clone(),
            idx: self.idx,
            line: self.line,
            is_comment: self.is_comment,
            reserved_keywords: RESERVED_KEYWORDS.iter().map(|k| *k).collect(),
        }
    }
}

// Peek the next item
impl<'input> Lexer<'input> {
    pub fn peek(&self) -> Option<&(Token<'input>, usize)> {
        self.clone().next()
    }
}
