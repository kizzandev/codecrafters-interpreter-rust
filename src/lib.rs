use std::{
    fs,
    io::{self, Write},
};

pub fn read_file(filename: &str) -> String {
    fs::read_to_string(filename).unwrap_or_else(|err| {
        writeln!(io::stderr(), "Problem opening the file: {}", err).unwrap();
        String::new()
    })
}
