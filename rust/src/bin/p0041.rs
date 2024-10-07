// Project Euler: Problem 41

euler::run_solver!(41);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    use euler::math::{self, primes};
    use itertools::Itertools;

    for k in [7_i64, 4] {
        for lst in (1..=k).rev().permutations(k as usize) {
            let n = lst.iter().fold(0, |acc, x| 10 * acc + x);
            if math::is_pandigital_nz(n) && primes::is_prime(n) {
                return n;
            }
        }
    }

    unreachable!();
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0041() {
        assert_eq!(compute(), 7652413);
    }
}
