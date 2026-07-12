// Project Euler: Problem 60

use euler::math::primes;
use std::collections::{HashMap, HashSet};

euler::run_solver!(60);

fn solve() -> String {
    compute(5).to_string()
}

fn compute(size_of_clique: usize) -> u64 {
    use std::cmp;

    debug_assert!(size_of_clique > 1);

    let mut prime_lst: Vec<Vec<u64>> = vec![vec![3], vec![3]];
    let mut tbl: HashMap<u64, HashSet<u64>> = HashMap::new();
    let mut ans = u64::MAX;

    // start searching from the 4th prime, 7
    let mut p: u64 = 5;
    loop {
        // break this loop when it has verified the answer is the lowest sum
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

        // if number of connectable primes is less than 'size_of_clique - 1', check the next larger prime.
        if nbr_lst.len() < size_of_clique - 1 {
            continue;
        }

        // switch the list of connectable primes to descending order and check cliques.
        nbr_lst.reverse();
        if let Some(clqs) = check_cliques(&nbr_lst, size_of_clique - 1, &tbl) {
            ans = cmp::min(
                ans,
                clqs.into_iter()
                    .map(|v| v.into_iter().sum::<u64>() + p)
                    .min()
                    .unwrap(),
            );
        }
    }

    ans
}

fn is_pair(x: u64, y: u64) -> bool {
    fn concat(a: u64, b: u64) -> u64 {
        let mut n: u64 = 10;
        while b > n {
            n *= 10;
        }
        a * n + b
    }

    primes::is_prime(concat(x, y)) && primes::is_prime(concat(y, x))
}

fn find_nbrs(p: u64, asc_p_lst: &[u64], current_ans: u64) -> Vec<u64> {
    asc_p_lst
        .iter()
        .take_while(|x| **x < current_ans - p)
        .filter(|x| is_pair(**x, p))
        .copied()
        .collect()
}

fn check_cliques(
    desc_nbr_lst: &[u64],
    size: usize,
    tbl: &HashMap<u64, HashSet<u64>>,
) -> Option<Vec<Vec<u64>>> {
    fn aux(
        group: &[u64],
        ps: &[u64],
        depth: usize,
        tbl: &HashMap<u64, HashSet<u64>>,
        result: &mut Vec<Vec<u64>>,
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

    let mut result: Vec<Vec<u64>> = Vec::new();
    aux(&Vec::new(), desc_nbr_lst, size, tbl, &mut result);

    if result.is_empty() {
        None
    } else {
        Some(result)
    }
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
