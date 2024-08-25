use std::process::ExitCode;

// use crate::ast::Expr;
// use crate::evaluator::{evaluate, Res};
// use crate::interpreter::interpret;
// use crate::lexer::RESERVED_KEYWORDS;
// use crate::tokenizer::tokenize;

use crate::parser::parse;

pub fn _interpret(file_contents: &str) -> ExitCode {
    // The code should be interpreted continuously until an error occurs or the end of the program is reached.

    loop {
        let _parser = parse(file_contents);
        break;
    }

    ExitCode::SUCCESS
}
