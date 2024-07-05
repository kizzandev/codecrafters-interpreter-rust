use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub struct Error {
    pub exit_code: u8,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {}", self.exit_code)
    }
}

impl Error {
    pub fn new(exit_code: u8) -> Self {
        Error { exit_code }
    }
}