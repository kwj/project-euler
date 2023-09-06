// Project Euler: Problem 60

use euler::math::primes;
use itertools::Itertools;
use std::collections::HashMap;

euler::run_solver!(60);

fn solve() -> String {
    compute(5).to_string()
}

fn compute(size_of_clique: usize) -> i64 {
    let mut prime_lst: Vec<Vec<i64>> = vec![vec![3], vec![3]];
    let mut tbl: HashMap<i64, Vec<i64>> = HashMap::new();
    let mut ans = i64::MAX;

    // start from the 4th prime, 7
    let mut p: i64 = 5;
    loop {
        p = primes::next_prime(p);
        // break this loop when it has verified the answer is smallest
        if p >= ans {
            break;
        }

        // find all prime numbers smaller than 'p' that can be paired with 'p'
        let idx = ((p + 2) % 3) as usize;
        let nbr_lst = find_nbrs(p, &prime_lst[idx], ans);
        tbl.insert(p, nbr_lst.clone());
        // update known prime numbers
        prime_lst[idx].insert(0, p);

        // if number of connectable primes is less than 'size_of_clique - 1', check the next prime.
        if nbr_lst.len() < size_of_clique - 1 {
            continue;
        }

        // I feel that it probably slows down at this loop since combination() is used.
        // Note: I haven't run profiling, so it is just a guess.
        for prime_grp in nbr_lst.into_iter().combinations(size_of_clique - 1) {
            let tmp = p + prime_grp.iter().copied().sum::<i64>();
            if tmp >= ans {
                continue;
            }
            if is_clique(&prime_grp, &tbl) {
                ans = std::cmp::min(tmp, ans);
            }
        }
    }
    ans
}

fn is_pair(x: i64, y: i64) -> bool {
    fn concat(a: i64, b: i64) -> i64 {
        let mut n: i64 = 10;
        while b > n {
            n *= 10;
        }
        a * n + b
    }

    primes::is_prime(concat(x, y)) && primes::is_prime(concat(y, x))
}

fn find_nbrs(p: i64, p_lst: &[i64], limit: i64) -> Vec<i64> {
    p_lst
        .iter()
        .filter(|&x| *x + p < limit && is_pair(*x, p))
        .copied()
        .collect::<Vec<_>>()
}

fn is_clique(p_grp: &[i64], tbl: &HashMap<i64, Vec<i64>>) -> bool {
    fn is_subset(a: &[i64], b: &[i64]) -> bool {
        a.iter().all(|x| b.contains(x))
    }

    for idx in 0..(p_grp.len() - 1) {
        if !is_subset(&p_grp[(idx + 1)..], tbl.get(&p_grp[idx]).unwrap()) {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0060_4() {
        assert_eq!(compute(4), 792);
    }

    #[test]
    fn p0060_5() {
        assert_eq!(compute(5), 26033);
    }
}
