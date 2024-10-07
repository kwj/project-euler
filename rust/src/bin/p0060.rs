// Project Euler: Problem 60

use euler::math::primes;
use std::cmp;
use std::collections::{HashMap, HashSet};

euler::run_solver!(60);

fn solve() -> String {
    compute(5).to_string()
}

fn compute(size_of_clique: usize) -> i64 {
    debug_assert!(size_of_clique > 1);

    let mut prime_lst: Vec<Vec<i64>> = vec![vec![3], vec![3]];
    let mut tbl: HashMap<i64, HashSet<i64>> = HashMap::new();
    let mut ans = i64::MAX;

    // start searching from the 4th prime, 7
    let mut p: i64 = 5;
    loop {
        // break this loop when it has verified the answer is smallest
        p = primes::next_prime(p);
        if p >= ans {
            break;
        }

        // search for prime numbers that can be paired with 'p' and are less than 'p'
        let idx = ((p % 3) - 1) as usize;
        let mut nbr_lst = find_nbrs(p, &prime_lst[idx], ans);
        tbl.insert(p, HashSet::from_iter(nbr_lst.clone()));

        // update checked prime numbers
        prime_lst[idx].push(p);

        // if number of connectable primes is less than 'size_of_clique - 1', check the next prime.
        if nbr_lst.len() < size_of_clique - 1 {
            continue;
        }

        // convert to descending order
        nbr_lst.reverse();

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

fn find_nbrs(p: i64, asc_p_lst: &[i64], current_ans: i64) -> Vec<i64> {
    asc_p_lst
        .iter()
        .take_while(|&x| *x + p < current_ans)
        .filter(|&x| is_pair(*x, p))
        .copied()
        .collect()
}

fn find_cliques(
    desc_nbr_lst: &[i64],
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
                if group
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
    aux(&Vec::new(), desc_nbr_lst, size, tbl, &mut result);
    result
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0060_3() {
        // 3, 37, 67
        assert_eq!(compute(3), 107);
    }

    #[test]
    fn p0060_4() {
        // 3, 7, 109, 673
        assert_eq!(compute(4), 792);
    }

    #[test]
    fn p0060_5() {
        // 13, 5197, 5701, 6733, 8389
        assert_eq!(compute(5), 26033);
    }
}
