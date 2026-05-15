// Project Euler: Problem 50

use euler::math::primes;

euler::run_solver!(50);

fn solve() -> String {
    compute(1_000_000).to_string()
}

fn compute(limit: i64) -> i64 {
    debug_assert!(limit > 2);

    let mut cs_gen: CumSumPrime = CumSumPrime::default();
    let mut cs_lst: Vec<i64> = cs_gen.initial_lst(limit);

    let mut k: usize = cs_lst.len() - 2;
    let mut left: usize = 0;
    loop {
        let diff = cs_lst[left + k] - cs_lst[left];
        if diff >= limit {
            left = 0;
            k -= 1;
        } else if primes::is_prime(diff) {
            return diff;
        } else {
            left += 1;
            if left + k >= cs_lst.len() {
                cs_lst.push(cs_gen.next().unwrap());
            }
        }
    }
}

#[derive(Default)]
struct CumSumPrime {
    cumsum: i64,
    prime: i64,
}

impl Iterator for CumSumPrime {
    type Item = i64;

    fn next(&mut self) -> Option<i64> {
        self.prime = primes::next_prime(self.prime);
        self.cumsum += self.prime;

        Some(self.cumsum)
    }
}

impl CumSumPrime {
    fn initial_lst(&mut self, limit: i64) -> Vec<i64> {
        use std::iter;

        [0].into_iter()
            .chain(iter::from_fn(|| {
                if self.cumsum < limit {
                    self.next()
                } else {
                    None
                }
            }))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0050_100() {
        assert_eq!(compute(100), 41);
    }

    #[test]
    fn p0050_500() {
        assert_eq!(compute(500), 499);
    }

    #[test]
    fn p0050_1000() {
        assert_eq!(compute(1_000), 953);
    }

    #[test]
    fn p0050_10000() {
        assert_eq!(compute(10_000), 9521);
    }

    #[test]
    fn p0050_1000000() {
        assert_eq!(compute(1_000_000), 997651);
    }
}
