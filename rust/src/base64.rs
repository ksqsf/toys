/// SIMD primitives (x86_64)
///
/// Reference: https://software.intel.com/sites/landingpage/IntrinsicsGuide/
///
/// Basic data types (of course we want the biggest)
/// * _mm64
/// * _mm128, _mm128i, _mm128d
/// * _mm256, _mm256i, _mm256d
/// * AVX-512 is not available on my machine, ugh
///
/// Vector types
/// * pi, epi: packed singed integers
/// * pu, epu: packed unsigned integers
/// * ps, pd: packed single/double-precision floating point numbers
///
/// Basic intrinsic format
/// * `<datatype>_<operation>_<elementtype>[<bit>]`
///
/// Arithmetic
/// * add, sub
/// * adds, subs
/// * addsub
/// * hadd, hsub
/// * hadds, hsubs
/// * mul, div
/// * sign
///
/// Cast (only for compiler): e.g. castpd_ps
///
/// Compare
/// * cmp
/// * cmpgt
/// * cmpeq
///
/// Convert: e.g. cvtepi32_epi64
///
/// Cryptography:
/// * crc32
///
/// Elementary math functions
/// * rcp: reciprocals
/// * rsqrt: 1/sqrt(x)
/// * sqrt
///
/// Uncategorized
/// * undefined
/// * zeroall, zeroupper
/// * movedup: duplicate even-positioned elements
/// * movldup, movhdup
///
/// Load
/// * broadcast
/// * i32gather, mask_i32gather, i64gather, mask_i64gather
/// * load, lddqu, mask_load, stream_load
///
/// Logical
/// * and, andnot, or, xor
/// * test_all_ones, test_all_zeros, test_mix_ones_zeros, testc, testnzc, testz
///
/// Stat
/// * avg
///
/// Set
/// * set
/// * set1
/// * setr
/// * setzero
///
/// Shift
/// * bslli: Shift left by imm8 bytes while shifting in zeros
/// * bsrli
/// * sll, slli, sllv, sra, srai, srav, srl, srli, srlv
///
/// Special functions
/// * max, min
/// * floor, ceil, round
/// * abs
///
/// String compare
/// * cmp[ei]str[acimosz]
///
/// Swizzle
/// * blend
/// * extract
/// * insert
/// * permute
/// * shuffle
/// * unpackhi, unpacklo

use std::arch::x86_64::*;

fn chr(x: u8) -> u8 {
    if x < 26 {
        x + b'A'
    } else if x >= 26 && x < 52 {
        x - 26 + b'a'
    } else if x >= 52 && x < 62 {
        x - 52 + b'0'
    } else if x == 62 {
        b'+'
    } else if x == 63 {
        b'/'
    } else {
        unreachable!()
    }
}

/// Naive BASE64 encoding.
pub fn encode(input: &[u8]) -> Vec<u8> {
    let mut result = Vec::with_capacity(input.len() * 4 / 3);
    for slice in input.chunks(3) {
        result.push(chr(slice[0] >> 2));
        if slice.len() == 3 {
            result.push(chr(((slice[0]&0x3)<<4) | (slice[1]>>4)));
            result.push(chr((slice[1]&0xf)<<2 | (slice[2]>>6)));
            result.push(chr(slice[2]&0x3f));
        } else if slice.len() == 2 {
            result.push(chr((slice[0]&0x3)<<4) | (slice[1]>>4));
            result.push(chr((slice[1]&0xf)<<2));
            result.push(b'=');
        } else if slice.len() == 1 {
            result.push(chr((slice[0]&0x3)<<4));
            result.push(b'=');
            result.push(b'=');
        }
    }
    result
}

/// SIMD BASE64 encoding.
pub fn encode_simd() {
    
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test::*;

    #[test]
    fn test_encode() {
        assert_eq!(&encode(b""), b"");
        assert_eq!(&encode(b"a"), b"YQ==");
        assert_eq!(&encode(b"hello"), b"aGVsbG8=");
        assert_eq!(&encode(b"hello1"), b"aGVsbG8x");
        assert_eq!(&encode(b"hell"), b"aGVsbA==");
    }

    #[bench]
    fn bench_encode_100(b: &mut Bencher) {
        let buf = [0; 100];
        b.iter(|| {
            encode(&buf);
        });
    }

    #[bench]
    fn bench_encode_1000(b: &mut Bencher) {
        let buf = [0; 1000];
        b.iter(|| {
            encode(&buf);
        });
    }

    #[bench]
    fn bench_encode_10k(b: &mut Bencher) {
        let buf = [0; 10000];
        b.iter(|| {
            encode(&buf);
        });
    }

    #[bench]
    fn memory_copy_10k(b: &mut Bencher) {
        let buf = [0; 10000];
        let mut dst = [0; 10000];
        b.iter(|| {
            dst = buf;
        });
    }
}
