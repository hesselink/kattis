use std::io::{Read,Result};

fn main() {
    let mut input = String::new();
    std::io::stdin().read_to_string(&mut input)?;
    input.lines()
        .next()
        .and_then(|line| line.split_whitespace().last())
        .map(|word| println!("{}", word));
}
