// Project Euler: Problem 7

euler::run_solver!(7);

fn solve() -> String {
    compute(10_001).to_string()
}

fn compute(n_th: usize) -> i64 {
    use euler::math::primes;
    use std::iter;

    debug_assert!(n_th > 0);

    iter::successors(Some(2_i64), |&p| Some(primes::next_prime(p)))
        .nth(n_th - 1)
        .unwrap()
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
