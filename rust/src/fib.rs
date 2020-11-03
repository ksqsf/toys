const MOD: u64 = 1e9 as u64 + 7;

#[derive(Copy, Clone)]
struct Mat(pub [u64; 4]);

impl std::ops::Mul for Mat {
    type Output = Mat;
    #[inline(always)]
    fn mul(self, other: Mat) -> Mat {
        Mat([
            (self.0[0] * other.0[0] + self.0[1] * other.0[2]) % MOD,
            (self.0[0] * other.0[1] + self.0[1] * other.0[3]) % MOD,
            (self.0[2] * other.0[0] + self.0[3] * other.0[2]) % MOD,
            (self.0[2] * other.0[1] + self.0[3] * other.0[3]) % MOD,
        ])
        // Mat([
        //     (self.0[0] * other.0[0] + self.0[1] * other.0[2]),
        //     (self.0[0] * other.0[1] + self.0[1] * other.0[3]),
        //     (self.0[2] * other.0[0] + self.0[3] * other.0[2]),
        //     (self.0[2] * other.0[1] + self.0[3] * other.0[3]),
        // ])
    }
}

pub fn fib(n: u64) -> u64 {
    let mut x = 0;
    let mut y = 1;
    for _ in 0..n {
        let t = (x + y) % MOD;
        // let t = x + y;
        x = y;
        y = t;
    }
    y
}

pub fn quick_fib(mut b: u64) -> u64 {
    let mut res = Mat([1, 0, 0, 1]);
    let mut a = Mat([1, 1, 1, 0]);
    while b > 0 {
        if b & 1 == 1 {
            res = res * a;
        }
        b = b >> 1;
        a = a * a;
    }
    res.0[0]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test::*;

    static N: u64 = 100000;

    #[test]
    fn fib_is_correct() {
        assert_eq!(fib(3), 3);
        assert_eq!(fib(4), 5);
        assert_eq!(fib(5), 8);
        for i in 0..10000 {
            assert_eq!(fib(i), quick_fib(i))
        }
    }

    #[bench]
    fn fib_iter(b: &mut Bencher) {
        let n = black_box(N);
        let mut ans = 0;
        b.iter(|| ans = fib(n))
    }

    #[bench]
    fn fib_quickpow(b: &mut Bencher) {
        let n = black_box(N);
        let mut ans = 0;
        b.iter(|| ans = quick_fib(n))
    }
}
