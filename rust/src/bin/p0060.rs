// Project Euler: Problem 60

use euler::math::primes;
use std::cmp;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;

euler::run_solver!(60);

fn solve() -> String {
    compute(5).to_string()
}

fn compute(size_of_clique: usize) -> i64 {
    let mut prime_lst: Vec<Vec<i64>> = vec![vec![3], vec![3]];
    let mut tbl: HashMap<i64, HashSet<i64>> = HashMap::new();
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
        tbl.insert(p, HashSet::from_iter(nbr_lst.clone()));
        // update known prime numbers
        prime_lst[idx].push(p);

        // if number of connectable primes is less than 'size_of_clique - 1', check the next prime.
        if nbr_lst.len() < size_of_clique - 1 {
            continue;
        }

        let cliques = find_cliques(&nbr_lst, size_of_clique - 1, &tbl);
        if !cliques.is_empty() {
            ans = cmp::min(
                ans,
                cliques
                    .into_iter()
                    .map(|v| v.iter().sum::<i64>() + p)
                    .min()
                    .unwrap(),
            );
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
        .rev()
        .filter(|&x| *x + p < limit && is_pair(*x, p))
        .copied()
        .collect::<Vec<_>>()
}

fn find_cliques(
    desc_ps_lst: &[i64],
    size: usize,
    tbl: &HashMap<i64, HashSet<i64>>,
) -> Vec<Vec<i64>> {
    fn aux(
        group: &[i64],
        ps: &[i64],
        depth: usize,
        tbl: &HashMap<i64, HashSet<i64>>,
        result: &mut Vec<Vec<i64>>,
    ) {
        if depth == 0 {
            result.push(group.to_vec());
        } else {
            for offset in 0..=(ps.len() - depth) {
                if group.is_empty()
                    || group
                        .iter()
                        .all(|x| tbl.get(x).unwrap().contains(&ps[offset]))
                {
                    let mut next_group = group.to_vec();
                    next_group.push(ps[offset]);
                    aux(&next_group, &ps[(offset + 1)..], depth - 1, tbl, result);
                }
            }
        }
    }
    let mut result: Vec<Vec<i64>> = Vec::new();
    aux(&Vec::new(), desc_ps_lst, size, tbl, &mut result);
    result
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
