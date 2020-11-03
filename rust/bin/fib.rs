pub mod fib;

use crate::fib::*;

fn main() {
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    let n: u64 = buf.trim().parse().unwrap();
    println!("{}", fib(n));
    println!("{}", quick_fib(n));
}
