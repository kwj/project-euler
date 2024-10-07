// Project Euler: Problem 20

euler::run_solver!(20);

fn solve() -> String {
    compute(100).to_string()
}

fn compute(n: usize) -> i64 {
    use num_bigint::BigUint;

    debug_assert!(n > 0);

    (1..=n)
        .fold(BigUint::from(1_u32), |acc, x| acc * x)
        .to_radix_le(10)
        .into_iter()
        .map(i64::from)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0020_10() {
        assert_eq!(compute(10), 27);
    }

    #[test]
    fn p0020_100() {
        assert_eq!(compute(100), 648);
    }
}
