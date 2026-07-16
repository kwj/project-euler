pub mod primes;

pub fn gcd(mut x: u64, mut y: u64) -> u64 {
    use std::cmp::min;

    if x == 0 {
        return y;
    } else if y == 0 {
        return x;
    }

    let x_zeros = x.trailing_zeros();
    let y_zeros = y.trailing_zeros();
    x >>= x_zeros;
    y >>= y_zeros;

    loop {
        if x >= y {
            x -= y;
            if x == 0 {
                return y << min(x_zeros, y_zeros);
            }
        } else {
            (x, y) = (y - x, x);
        }

        x >>= x.trailing_zeros();
    }
}

pub fn lcm(x: u64, y: u64) -> u64 {
    if x == 0 || y == 0 {
        return 0;
    }
    x * (y / gcd(y, x))
}

pub fn bit_length(n: u64) -> u32 {
    fn aux(n: u64, cnt: u32) -> u32 {
        if n == 0 { cnt } else { aux(n / 2, cnt + 1) }
    }

    aux(n, 0)
}

/// # Panics
///
/// Will panic if n is less than 2
pub fn prime_factors(mut n: u64) -> Vec<u64> {
    assert!(n > 1);

    let mut result: Vec<u64> = Vec::new();
    for b in [2, 3, 5] {
        while n.is_multiple_of(b) {
            result.push(b);
            n /= b;
        }
    }

    if n != 1 {
        let diff = [4, 2, 4, 2, 4, 6, 2, 6];
        let mut b = 7;
        let mut idx = 0;

        while n / b >= b {
            if n.is_multiple_of(b) {
                result.push(b);
                n /= b;
            } else {
                b += diff[idx];
                idx = (idx + 1) % diff.len();
            }
        }
        result.push(n);
    }

    result
}

pub fn factorize(n: u64) -> Vec<(u64, u32)> {
    let lst = prime_factors(n);
    let mut b = lst[0];
    let mut e = 1_u32;
    let mut result: Vec<(u64, u32)> = Vec::new();

    for &x in &lst[1..] {
        if x == b {
            e += 1;
        } else {
            result.push((b, e));
            b = x;
            e = 1;
        }
    }
    result.push((b, e));

    result
}

pub fn divisors(num: u64) -> Vec<u64> {
    let mut lst: Vec<u64> = vec![1];
    for (b, e) in factorize(num) {
        let mut acc_lst: Vec<u64> = Vec::new();
        for m in (1..=e).map(|x| b.pow(x)) {
            lst.iter().map(|x| x * m).for_each(|x| acc_lst.push(x));
        }
        lst.extend(acc_lst);
    }
    lst.sort();
    lst
}

pub fn proper_divisors(num: u64) -> Vec<u64> {
    let mut result = divisors(num);
    result.pop();
    result
}

pub fn binomial(n: u64, k: u64) -> u64 {
    ((n - k + 1)..=n)
        .zip(1..=k)
        .fold(1, |acc, (x, y)| acc * x / y)
}

pub fn is_palindrome(num: u64, base: u64) -> bool {
    let mut x = num;
    let mut acc = 0;
    while x > 0 {
        acc = acc * base + (x % base);
        x /= base;
    }
    acc == num
}

pub fn is_pandigital(num: u64) -> bool {
    fn mk_bits(mut n: u64) -> u32 {
        let mut bits = 0_u32;
        while n > 0 {
            bits |= 1_u32 << &(n % 10);
            n /= 10;
        }
        bits
    }

    mk_bits(num) == (1_u32 << num_of_digits(num, 10)) - 1
}

pub fn is_pandigital_nz(num: u64) -> bool {
    fn check_zero(mut n: u64) -> bool {
        while n > 0 {
            if n.is_multiple_of(10) {
                return false;
            }
            n /= 10;
        }
        true
    }

    check_zero(num) && is_pandigital(num * 10)
}

pub fn powmod(base: u64, mut exp: u64, modulo: u64) -> u64 {
    let mut ans: u128 = 1;
    let mut b = u128::from(base);
    let m = u128::from(modulo);

    b %= m;
    while exp > 0 {
        if exp & 1 == 1 {
            ans = (ans * b) % m;
        }
        b = (b * b) % m;
        exp >>= 1;
    }

    u64::try_from(ans).unwrap()
}

pub fn digits(mut num: u64) -> Vec<u64> {
    let mut lst: Vec<u64> = Vec::new();
    loop {
        lst.push(num % 10);
        num /= 10;
        if num == 0 {
            break;
        }
    }
    lst
}

pub fn undigits(lst: &[u64]) -> u64 {
    lst.iter().rev().fold(0, |acc, x| acc * 10 + *x)
}

pub fn sigma_tbl(z: u32, upper: usize) -> Vec<u64> {
    use primes::primes;

    let p_lst = primes(1, u64::try_from(upper).unwrap())
        .iter()
        .map(|x| usize::try_from(*x).unwrap())
        .collect::<Vec<_>>();
    let mut result: Vec<u64> = vec![1; upper + 1];

    for p in &p_lst {
        let mut q = *p;
        let mut x = 0_u64;
        while q <= upper {
            x += q.pow(z) as u64;
            result[q] += x;
            q *= *p;
        }
    }

    for p in &p_lst {
        let mut q = *p;
        while q <= upper {
            for n in 2..=(upper / q) {
                if result[n] == 1 || n % p == 0 {
                    continue;
                }
                result[q * n] = result[q] * result[n];
            }
            q *= *p;
        }
    }

    result[0] = 0;
    result
}

pub fn aliquot_sum_tbl(upper: usize) -> Vec<u64> {
    sigma_tbl(1, upper)
        .into_iter()
        .enumerate()
        .map(|(idx, x)| x - idx as u64)
        .collect()
}

pub fn get_max_exp(mut n: u64, base: u64) -> u32 {
    let mut e = 0;
    while n >= base {
        n /= base;
        e += 1;
    }
    e
}

pub fn num_of_digits(num: u64, base: u64) -> u32 {
    get_max_exp(num, base) + 1
}

pub fn is_triangular(num: u64) -> bool {
    let tmp = 8 * num + 1;
    let tmp_sqrt = tmp.isqrt();

    tmp_sqrt * tmp_sqrt == tmp && tmp_sqrt % 2 == 1
}

pub fn is_square(num: u64) -> bool {
    let num_sqrt = num.isqrt();

    num_sqrt * num_sqrt == num
}

pub fn is_pentagonal(num: u64) -> bool {
    let tmp = 24 * num + 1;
    let tmp_sqrt = tmp.isqrt();

    tmp_sqrt * tmp_sqrt == tmp && tmp_sqrt % 6 == 5
}

pub fn is_hexagonal(num: u64) -> bool {
    let tmp = 8 * num + 1;
    let tmp_sqrt = tmp.isqrt();

    tmp_sqrt * tmp_sqrt == tmp && tmp_sqrt % 4 == 3
}
