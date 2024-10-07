// Project Euler: Problem 50

use euler::math::primes;

euler::run_solver!(50);

fn solve() -> String {
    compute(1_000_000).to_string()
}

fn compute(limit: i64) -> i64 {
    let mut cs_gen: CumSumPrime = Default::default();
    let mut cs_lst: Vec<i64> = cs_gen.initial_lst(limit);
    let mut ans: i64 = 0;
    let mut left: usize = 0;
    let mut k: usize = 0;

    while cs_lst[left + k] - cs_lst[left] < limit {
        let base = cs_lst[left];
        if let Some(tpl) = cs_lst[(left + k)..]
            .iter()
            .enumerate()
            .skip(1)
            .rev()
            .find(|(_, &p)| p - base < limit && primes::is_prime(p - base))
        {
            k += tpl.0;
            ans = tpl.1 - base;
        }
        cs_lst.push(cs_gen.next().unwrap());
        left += 1;
    }

    ans
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

        let mut lst: Vec<_> = iter::from_fn(|| {
            if self.cumsum < limit {
                self.next()
            } else {
                None
            }
        })
        .collect();

        lst.insert(0, 0);
        lst
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
