// Project Euler: Problem 46

euler::run_solver!(46);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    use euler::math::{self, primes};

    fn is_twice_square(n: i64) -> bool {
        n % 2 == 0 && math::isqrt(n / 2).pow(2) == n / 2
    }

    // Two is not odd
    let mut odd_primes: Vec<i64> = vec![3, 5, 7, 11, 13, 17, 19, 23, 29, 31];

    for x in (35..).step_by(2) {
        if primes::is_prime(x) {
            odd_primes.push(x);
        } else if !odd_primes.iter().any(|&p| is_twice_square(x - p)) {
            return x;
        }
    }

    // Not reached on this problem
    unreachable!();
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0046() {
        assert_eq!(compute(), 5777);
    }
}
