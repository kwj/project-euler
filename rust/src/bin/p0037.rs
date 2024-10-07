// Project Euler: Problem 37

euler::run_solver!(37);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    make_cands()
        .into_iter()
        .filter(|&x| check_left_truncatable(x))
        .sum()
}

fn make_cands() -> Vec<i64> {
    use euler::math::primes;
    use itertools::Itertools;

    let mut primes: Vec<i64> = vec![2, 3, 5, 7];
    let mut cands: Vec<i64> = vec![2, 3, 5, 7];

    while !cands.is_empty() {
        cands = cands
            .into_iter()
            .cartesian_product([1, 3, 7, 9])
            .map(|(x, y)| 10 * x + y)
            .filter(|&x| primes::is_prime(x))
            .collect();
        primes.extend(cands.clone());
    }

    primes
        .into_iter()
        .cartesian_product([3, 7])
        .map(|(x, y)| 10 * x + y)
        .filter(|&x| primes::is_prime(x))
        .collect()
}

fn check_left_truncatable(n: i64) -> bool {
    use euler::math::{self, primes};

    let mut d = math::num_of_digits(n, 10) as u32;

    while d > 0 {
        if !primes::is_prime(n % (10_i64.pow(d))) {
            return false;
        }
        d -= 1;
    }

    true
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0037() {
        assert_eq!(compute(), 748317);
    }
}
