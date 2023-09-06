// Project Euler: Problem 10

euler::run_solver!(10);

fn solve() -> String {
    compute(2_000_000).to_string()
}

fn compute(upper: i64) -> i64 {
    use euler::math::primes;

    primes::primes(1, upper).iter().sum()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0010_upper_10() {
        assert_eq!(compute(10), 17);
    }

    #[test]
    fn p0010_upper_2000000() {
        assert_eq!(compute(2_000_000), 142913828922);
    }
}
