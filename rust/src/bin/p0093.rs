// Project Euler: Problem 93

use itertools::Itertools;
use num_rational::Ratio;
use std::collections::HashSet;

euler::run_solver!(93);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i32 {
    let res = (1..=9)
        .combinations(4)
        .map(|lst| (get_consec_counts(&lst), lst))
        .max_by_key(|x| x.0)
        .unwrap();

    (res.1).iter().fold(0, |acc, n| acc * 10 + *n)
}

fn get_consec_counts(lst: &[i32]) -> i32 {
    let number_set = make_numbers(lst);
    for cnt in 1_i32.. {
        if number_set.contains(&cnt) {
            continue;
        }
        return cnt - 1;
    }

    unreachable!();
}

fn make_numbers(lst: &[i32]) -> HashSet<i32> {
    fn aux(lst: &[Ratio<i32>], res: &mut HashSet<i32>) {
        if lst.len() == 1 {
            if lst[0].is_integer() {
                res.insert(*lst[0].numer());
            }
        } else {
            for (i, &d1) in lst.iter().enumerate() {
                for (j, &d2) in lst[(i + 1)..].iter().enumerate() {
                    let mut next_lst = lst.to_vec();
                    next_lst.remove(i);
                    next_lst.remove(i + j);
                    aux(&[&[d1 + d2], &next_lst[..]].concat(), res);
                    aux(&[&[d1 * d2], &next_lst[..]].concat(), res);
                    aux(&[&[d1 - d2], &next_lst[..]].concat(), res);
                    aux(&[&[d2 - d1], &next_lst[..]].concat(), res);
                    if d1 != Ratio::new(0, 1) {
                        aux(&[&[d2 / d1], &next_lst[..]].concat(), res);
                    }
                    if d2 != Ratio::new(0, 1) {
                        aux(&[&[d1 / d2], &next_lst[..]].concat(), res);
                    }
                }
            }
        }
    }

    let rat_nums: Vec<_> = lst.iter().map(|n| Ratio::new(*n, 1)).collect();
    let mut res: HashSet<i32> = HashSet::new();
    aux(&rat_nums, &mut res);

    res
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0093() {
        assert_eq!(compute(), 1258);
    }
}
