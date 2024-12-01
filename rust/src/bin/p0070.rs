// Project Euler: Problem 70

/*
  The answer must be a composite number because prime 'n' is not a permutation of phi(n) = n - 1.

  n = p1^k1 * p2^k2 * p3^k3 * ... * p{r}^k{n}
  -->
    phi(N) = N * (1-1/p1) * (1-1/p2) * (1-1/p3) * ... * (1-1/p{n})
      <-->
    N/phi(N) = (p1/(p1-1)) * (p2/(p2-1)) * (p3/(p3-1)) * ... * (p{n}/(p{n}-1))

  From the problem statement, 87109/phi(87109) = 87109 / 79180 = 1.1001.
  11/10 = 1.1 and 7/6 = 1.666..., so 11 <= prime numbers <= 9_999_999 / 11 = 909090.8181...

  The answer N has the following form (p_i are prime numbers)

    N = p1^k1 * p2^k2 * ... * pn^kn  (N < 10^7, n > 1, 11 <= p1 < p2 < ... < pn, k1>2 when n=1)
*/

use euler::math::{self, primes};
use std::{cmp::Ordering, collections::BinaryHeap};

euler::run_solver!(70);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    use std::cmp;

    let limit = 10_000_000 - 1;
    let mut pq: BinaryHeap<Ratio> = BinaryHeap::new();
    pq.push(Ratio {
        priority: ratio(87109, 79180),
        num: 87109,
    });

    for p in primes::primes(11, math::isqrt(limit)).into_iter().rev() {
        if get_phi_ratio(&[(p, 1)]) > pq.peek().unwrap().priority {
            break;
        }

        for pf_lst in PrimeFactorization::new((p, primes::prev_prime(limit / p + 1)), limit) {
            if get_phi_ratio(&pf_lst[0..cmp::min(2, pf_lst.len())]) > pq.peek().unwrap().priority {
                break;
            }
            if is_perm(prod(&pf_lst), phi(&pf_lst)) {
                pq.push(Ratio {
                    priority: get_phi_ratio(&pf_lst),
                    num: prod(&pf_lst),
                });
            }
        }
    }

    pq.peek().unwrap().num
}

fn prod(pf_lst: &[(i64, i64)]) -> i64 {
    pf_lst.iter().fold(1, |acc, x| acc * (x.0).pow(x.1 as u32))
}

fn phi(pf_lst: &[(i64, i64)]) -> i64 {
    pf_lst
        .iter()
        .fold(1, |acc, x| acc * (x.0).pow(x.1 as u32 - 1) * (x.0 - 1))
}

fn get_phi_ratio(pf_lst: &[(i64, i64)]) -> f32 {
    ratio(prod(pf_lst), phi(pf_lst))
}

fn ratio(num: i64, denom: i64) -> f32 {
    num as f32 / denom as f32
}

// minimum binary heap (priority key: n/phi(n), value: n)
#[derive(Debug, Clone, PartialEq)]
struct Ratio {
    priority: f32,
    num: i64,
}

impl Eq for Ratio {}

impl Ord for Ratio {
    fn cmp(&self, other: &Self) -> Ordering {
        other.priority.partial_cmp(&self.priority).unwrap()
    }
}

impl PartialOrd for Ratio {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// prime factorization of candidates
#[derive(Debug)]
struct PrimeFactorization {
    pf_lst: Vec<(i64, i64)>,
    limit: i64,
}

impl PrimeFactorization {
    fn new(tpl: (i64, i64), limit: i64) -> PrimeFactorization {
        if tpl.0 == tpl.1 {
            PrimeFactorization {
                pf_lst: vec![(tpl.0, 2)],
                limit,
            }
        } else {
            PrimeFactorization {
                pf_lst: vec![(tpl.1, 1), (tpl.0, 1)],
                limit,
            }
        }
    }
}

impl Iterator for PrimeFactorization {
    type Item = Vec<(i64, i64)>;

    fn next(&mut self) -> Option<Vec<(i64, i64)>> {
        fn aux(lst: &[(i64, i64)], limit: i64) -> Vec<(i64, i64)> {
            let (b, e) = lst[0];
            let tmp = limit / prod(lst);
            if tmp < b {
                lst.to_vec()
            } else {
                let prev_p = primes::prev_prime(tmp);
                if prev_p > b {
                    [&[(prev_p, 1_i64)], lst].concat()
                } else {
                    [&[(b, e + 1)], &lst[1..]].concat()
                }
            }
        }

        let (b, e) = self.pf_lst[0];
        if self.pf_lst.len() == 1 && e == 1 {
            return None;
        }

        let mut result = self.pf_lst.clone();
        result.reverse();
        if e > 1 {
            self.pf_lst[0] = (b, e - 1);
        } else {
            let (new_b, new_e) = self.pf_lst[1];
            let prev_p = primes::prev_prime(b);
            if prev_p == new_b {
                self.pf_lst = aux(
                    &[&[(new_b, new_e + 1)], &self.pf_lst[2..]].concat(),
                    self.limit,
                );
            } else {
                self.pf_lst = aux(
                    &[&[(prev_p, 1_i64)], &self.pf_lst[1..]].concat(),
                    self.limit,
                );
            }
        }

        Some(result)
    }
}

fn is_perm(x: i64, y: i64) -> bool {
    let mut tmp_x = math::digits(x);
    let mut tmp_y = math::digits(y);

    tmp_x.sort_unstable();
    tmp_y.sort_unstable();
    tmp_x == tmp_y
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0070() {
        assert_eq!(compute(), 8319823);
    }
}
