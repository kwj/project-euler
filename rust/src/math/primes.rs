// Deterministic variants of the Miller-Rabin primality test (n <= 2 ** 64)
// http://miller-rabin.appspot.com/

pub fn is_prime(n: i64) -> bool {
    #[derive(PartialEq)]
    enum NumType {
        Prime,
        Composite,
        Undecided,
    }

    fn distinguish(a: i64, d: i64, s: i64, n: i64) -> NumType {
        use crate::math;
        let mut x = math::powmod(a, d, n);
        if x == 0 {
            NumType::Prime
        } else if x == 1 || x == n - 1 {
            NumType::Undecided
        } else {
            for _ in 0..s {
                x = math::powmod(x, 2, n);
                if x == n - 1 {
                    return NumType::Undecided;
                }
            }
            NumType::Composite
        }
    }

    if n < 2 || ((n % 6 != 1) && (n % 6 != 5)) {
        return n == 2 || n == 3;
    }

    let mut d = n - 1;
    let mut s = 0;
    while d % 2 == 0 {
        (d, s) = (d / 2, s + 1);
    }

    for a in [2, 325, 9375, 28178, 450775, 9780504, 1795265022].into_iter() {
        let result = distinguish(a, d, s, n);
        if result == NumType::Prime {
            return true;
        } else if result == NumType::Composite {
            return false;
        }
    }

    true
}

// Prime sieve by Wheel factorization
// https://en.wikipedia.org/wiki/Wheel_factorization

// ..=7:    0
// 8..=11:  1
// 12..=13: 2
// 14..=17: 3
// 18..=19: 4
// 20..=23: 5
// 24..=29: 6
// 30..=31: 7
//
// 32..=37: 0
// 38..=41: 1
// .....

static WHEEL: [i64; 8] = [7, 11, 13, 17, 19, 23, 29, 31];
static WHEEL_GAP: [i64; 8] = [4, 2, 4, 2, 4, 6, 2, 6];

fn num_to_index(n: i64) -> usize {
    const INDICES: [usize; 30] = [
        0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7,
    ];
    if n < 2 {
        return 0;
    }
    let q = (n - 2) as usize / 30;
    let r = (n - 2) as usize % 30;
    8 * q + INDICES[r]
}

fn index_to_num(idx: usize) -> i64 {
    let q = idx as i64 / 8;
    let r = idx % 8;

    30 * q + WHEEL[r]
}

fn get_small_sieve(upper: i64) -> Vec<bool> {
    use crate::math;

    assert!(upper >= 7, "'upper' must be larger or equal to seven");
    let max_index = num_to_index(upper + 1) - 1;
    let max_prime = index_to_num(max_index);
    let mut s_sieve = vec![true; max_index + 1];

    if upper >= 7 * 7 {
        for i in 0..=(num_to_index(math::isqrt(upper))) {
            if s_sieve[i] {
                let prime = index_to_num(i);
                let mut q = prime.pow(2);
                let mut idx = i % 8;
                while q <= max_prime {
                    s_sieve[num_to_index(q)] = false;
                    q += WHEEL_GAP[idx] * prime;
                    idx = (idx + 1) % 8;
                }
            }
        }
    }
    s_sieve
}

fn get_sieve(low: i64, high: i64) -> Vec<bool> {
    use crate::math;
    use std::cmp;

    assert!(high >= low, "'high' must be larger or equal to 'low'");
    assert!(low >= 7, "'low' must be larger or equal to seven");

    if low == 7 {
        return get_small_sieve(high);
    }

    let w_low = num_to_index(low);
    let w_high = num_to_index(high + 1) - 1;
    let max_prime = index_to_num(w_high);
    let mut sieve = vec![true; w_high - w_low + 1];

    if high >= 7 * 7 {
        let small_sieve = get_small_sieve(math::isqrt(high));
        for (i, val) in small_sieve.iter().enumerate() {
            if *val {
                let prime = index_to_num(i);
                let mut idx = num_to_index(cmp::max((low + prime - 1) / prime, prime));
                let mut q = prime.checked_mul(index_to_num(idx)).unwrap();
                if q > max_prime {
                    continue;
                }
                while q <= max_prime && q.is_positive() {
                    sieve[num_to_index(q) - w_low] = false;
                    idx %= 8;
                    q += WHEEL_GAP[idx] * prime;
                    idx += 1;
                }
            }
        }
    }
    sieve
}

pub fn primes(mut low: i64, high: i64) -> Vec<i64> {
    use std::cmp;

    assert!(high >= low, "'high' must be larger or equal to 'low'");

    let mut lst: Vec<i64> = Vec::new();
    if low <= 2 && 2 <= high {
        lst.push(2);
    }
    if low <= 3 && 3 <= high {
        lst.push(3);
    }
    if low <= 5 && 5 <= high {
        lst.push(5);
    }
    if high <= 6 {
        return lst;
    }
    low = cmp::max(low, 7);
    let w_offset = num_to_index(low);
    for (i, val) in get_sieve(low, high).iter().enumerate() {
        if *val {
            lst.push(index_to_num(i + w_offset));
        }
    }
    lst
}

pub fn next_prime(n: i64) -> i64 {
    if n < 2 {
        2
    } else if n < 3 {
        3
    } else if n < 5 {
        5
    } else if n < 7 {
        7
    } else {
        let mut idx = num_to_index(n) + 1;
        let mut x: i64;
        loop {
            x = index_to_num(idx);
            if is_prime(x) {
                break;
            }
            idx += 1;
        }
        x
    }
}

pub fn prev_prime(n: i64) -> i64 {
    assert!(n > 2, "The argument must be larger than two.");

    if n <= 3 {
        2
    } else if n <= 5 {
        3
    } else if n <= 7 {
        5
    } else {
        let mut idx = num_to_index(n) - 1;
        let mut x: i64;
        loop {
            x = index_to_num(idx);
            if is_prime(x) {
                break;
            }
            idx -= 1;
        }
        x
    }
}
