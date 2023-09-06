// Project Euler: Problem 49

use std::collections::HashMap;

euler::run_solver!(49);

fn solve() -> String {
    compute(4).to_string()
}

fn compute(n_digits: u32) -> i64 {
    let p_tbl = get_prime_tbl(n_digits);
    for lst in p_tbl.values() {
        let length = lst.len();
        if length < 3 {
            continue;
        }

        for i in 0..(length - 2) {
            for j in (i + 1)..(length - 1) {
                let tmp = lst[j] * 2 - lst[i];
                if lst.contains(&tmp) && lst[i] != 1487 {
                    return [lst[i], lst[j], tmp]
                        .into_iter()
                        .fold(0, |acc, x| acc * 10_i64.pow(n_digits) + x);
                }
            }
        }
    }

    // Not reached on this problem
    unreachable!();
}

fn get_prime_tbl(n_digits: u32) -> HashMap<i64, Vec<i64>> {
    use euler::math::{self, primes};
    use std::cmp;

    let mut p_tbl: HashMap<i64, Vec<i64>> = HashMap::new();

    for p in primes::primes(10_i64.pow(n_digits - 1), 10_i64.pow(n_digits)) {
        let mut key_digits = math::digits(p);
        key_digits.sort_by_key(|&x| cmp::Reverse(x));
        let key = math::undigits(&key_digits);
        let mut lst = match p_tbl.get(&key) {
            Some(v) => v.clone(),
            None => Vec::new(),
        };
        lst.push(p);
        p_tbl.insert(key, lst);
    }
    p_tbl
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0049_ndigits_4() {
        assert_eq!(compute(4), 296962999629);
    }
}
