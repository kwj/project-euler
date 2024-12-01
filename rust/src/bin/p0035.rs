// Project Euler: Problem 35

euler::run_solver!(35);

fn solve() -> String {
    compute(1_000_000).to_string()
}

fn compute(limit: i64) -> usize {
    use euler::math::{self, primes};

    fn check_rot_num(n: i64) -> bool {
        let s = &format!("{n}{n}");
        let m = math::num_of_digits(n, 10) as usize;
        (0..m)
            .map(|pos| s[pos..(pos + m)].parse::<i64>().unwrap())
            .all(primes::is_prime)
    }

    primes::primes(1, limit)
        .into_iter()
        .filter(|&x| check_rot_num(x))
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
