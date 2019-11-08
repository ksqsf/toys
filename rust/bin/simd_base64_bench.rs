use rust::base64::*;

use std::arch::x86_64::*;
use std::fs::*;
use std::io::*;
use rand::prelude::*;

fn gen_input() -> Vec<u8> {
    const SIZE: usize = 1024*1024*1024;
    let mut result = Vec::with_capacity(SIZE);
    let mut rng = rand::thread_rng();
    for _ in 0..SIZE {
        let c: u64 = rng.gen();
        result.push((c & 0xff) as u8);
        result.push(((c >> 8) & 0xff) as u8);
        result.push(((c >> 16) & 0xff) as u8);
        result.push(((c >> 24) & 0xff) as u8);
    }
    result
}

fn main() {
    gen_input();
    
}
