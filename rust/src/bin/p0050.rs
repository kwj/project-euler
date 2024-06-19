// Project Euler: Problem 50

use euler::math::primes;

euler::run_solver!(50);

fn solve() -> String {
    compute(1_000_000).to_string()
}

fn compute(limit: i64) -> i64 {
    let mut cs_gen: CumSumPrime = Default::default();
    let mut cs_lst: Vec<i64> = cs_gen.get_initial_lst(limit);

    let mut ans: i64 = 0;
    let mut i: usize = 0;
    let mut consec_length: usize = 0;
    while cs_lst[i + consec_length] - cs_lst[i] < limit {
        let begin = cs_lst[i];
        let lst: Vec<i64> = cs_lst[(i + consec_length)..]
            .iter()
            .rev()
            .skip_while(|&&p| p - begin >= limit || !primes::is_prime(p - begin))
            .copied()
            .collect();
        if !lst.is_empty() {
            consec_length += lst.len() - 1;
            ans = lst[0] - begin;
        }
        cs_lst.push(cs_gen.next().unwrap());
        i += 1;
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
    fn get_initial_lst(&mut self, limit: i64) -> Vec<i64> {
        let mut lst: Vec<i64> = vec![0];

        while *(lst.last().unwrap()) < limit {
            lst.push(self.next().unwrap());
        }
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
