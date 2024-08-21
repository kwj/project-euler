// Project Euler: Problem 7

euler::run_solver!(7);

fn solve() -> String {
    compute(10_001).to_string()
}

fn compute(nth: i64) -> i64 {
    use euler::math::primes;

    (0..nth).fold(1, |x, _| primes::next_prime(x))
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0007_the_6th_prime() {
        assert_eq!(compute(6), 13);
    }

    #[test]
    fn p0007_the_10001th_prime() {
        assert_eq!(compute(10_001), 104743);
    }
}
