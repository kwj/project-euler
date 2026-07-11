// Project Euler: Problem 36

euler::run_solver!(36);

fn solve() -> String {
    compute(1_000_000).to_string()
}

fn compute(limit: u64) -> u64 {
    use euler::math;

    fn check_palindrome(n: u64) -> bool {
        math::is_palindrome(n, 10) && math::is_palindrome(n, 2)
    }

    debug_assert!(limit > 1);

    (1..limit).step_by(2).filter(|x| check_palindrome(*x)).sum()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0036_limit_1000000() {
        assert_eq!(compute(1_000_000), 872187);
    }
}
