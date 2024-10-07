// Project Euler: Problem 55

use num_bigint::BigUint;

euler::run_solver!(55);

fn solve() -> String {
    compute(10_000).to_string()
}

fn compute(upper: u32) -> usize {
    debug_assert!(upper > 0);

    (1..=upper)
        .filter(|&n| is_rychrel(BigUint::from(n)))
        .count()
}

fn is_rychrel(mut n: BigUint) -> bool {
    let mut tmp = rev_biguint(&n);
    for _ in 0..50 {
        n += tmp;
        tmp = rev_biguint(&n);
        if n == tmp {
            return false;
        }
    }

    true
}

fn rev_biguint(n: &BigUint) -> BigUint {
    let rev_string = |s: &str| s.chars().rev().collect::<String>();

    BigUint::parse_bytes(rev_string(&BigUint::to_str_radix(n, 10)).as_bytes(), 10).unwrap()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0055_upper_10000() {
        assert_eq!(compute(10_000), 249);
    }
}
