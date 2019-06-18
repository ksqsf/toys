extern crate pest;

#[macro_use]
extern crate pest_derive;

use std::fs;
use pest::Parser;

#[derive(Parser)]
#[grammar = "c1.pest"]
pub struct C1Parser;

fn main() {
    let file = fs::read_to_string("test/simple.c1").expect("read file");
    let file = C1Parser::parse(Rule::file, &file).expect("unsuccessful parse")
        .next().unwrap();
    println!("{:#?}", file);
    dbg!(file);
}
