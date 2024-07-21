use std::{io, fs};

pub fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

pub fn read_file(filename: &str) -> String {
    fs::read_to_string(filename).unwrap_or_else(|err| {
        writeln!(io::stderr(), "Problem opening the file: {}", err).unwrap();
        String::new()
    })
}
