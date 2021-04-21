use rayon::prelude::*;

fn main() {
    let v: Vec<i32> = vec![-1212121, 321, 1321, 3781, 31231, 5431, 42341, 3214321];
    let m = v.par_iter().copied().reduce(|| i32::MIN, |m, x| m.max(x));
    println!("{}", m);
}
