// Project Euler: Problem 35

euler::run_solver!(35);

fn solve() -> String {
    compute(1_000_000).to_string()
}

fn compute(limit: u64) -> usize {
    use euler::math::{self, primes};
    use std::iter;

    fn check_circular_numbers(n: u64) -> bool {
        let k = math::get_max_exp(n, 10);
        let m = 10_u64.pow(k);
        let mut num = n;
        let circular_numbers = iter::from_fn(move || {
            let (q, r) = (num / m, num % m);
            num = r * 10 + q;
            Some(num)
        });

        circular_numbers.take(k as usize).all(primes::is_prime)
    }

    primes::primes(1, limit)
        .into_iter()
        .filter(|x| check_circular_numbers(*x))
        .count()
}

#[cfg(test)]
mod tests {
    use super::compute;
    #[test]
    fn p0035_100() {
        assert_eq!(compute(100), 13);
    }
    #[test]
    fn p0035_1000000() {
        assert_eq!(compute(1_000_000), 55);
    }
}
