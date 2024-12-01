// Project Euler: Problem 3

euler::run_solver!(3);

fn solve() -> String {
    compute(600_851_475_143).to_string()
}

fn compute(n: i64) -> i64 {
    use euler::math;

    debug_assert!(n > 0);

    *math::prime_factors(n).last().unwrap()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0003_13195() {
        assert_eq!(compute(13_195), 29);
    }

    #[test]
    fn p0003_600851475143() {
        assert_eq!(compute(600_851_475_143), 6_857);
    }
}
