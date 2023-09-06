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

    // Not reached on this problem
    unreachable!();
}

fn make_numbers(lst: &[i32]) -> HashSet<i32> {
    fn aux(lst: &[Ratio<i32>], res: &mut HashSet<i32>) {
        if lst.len() == 1 && lst[0].is_integer() {
            res.insert(*lst[0].numer());
        }

        for (i, d1) in lst.iter().enumerate() {
            for d2 in lst[(i + 1)..].iter() {
                let mut next_lst = lst.clone().to_vec();
                next_lst.retain(|x| *x != *d1 && *x != *d2);
                aux(&[&next_lst[..], &[*d1 + *d2]].concat(), res);
                aux(&[&next_lst[..], &[*d1 * *d2]].concat(), res);
                aux(&[&next_lst[..], &[*d1 - *d2]].concat(), res);
                aux(&[&next_lst[..], &[*d2 - *d1]].concat(), res);
                if *d1 != Ratio::new(0, 1) {
                    aux(&[&next_lst[..], &[*d2 / *d1]].concat(), res);
                }
                if *d2 != Ratio::new(0, 1) {
                    aux(&[&next_lst[..], &[*d1 / *d2]].concat(), res);
                }
            }
        }
    }

    let rat_nums = lst
        .iter()
        .map(|n| Ratio::new(*n, 1))
        .collect::<Vec<Ratio<i32>>>();
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
