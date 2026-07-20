// Project Euler: Problem 72

/*
  https://mathproblems123.wordpress.com/2018/05/10/sum-of-the-euler-totient-function/
*/

use std::collections::HashMap;

euler::run_solver!(72);

fn solve() -> String {
    compute(1_000_000).to_string()
}

fn compute(limit: u64) -> u64 {
    debug_assert!(limit > 1);

    let mut cache: HashMap<u64, u64> = HashMap::new();

    sum_phi(limit, &mut cache) - sum_phi(1, &mut cache)
}

fn sum_phi(num: u64, cache: &mut HashMap<u64, u64>) -> u64 {
    if let Some(&val) = cache.get(&num) {
        val
    } else {
        let mut v = num * (num + 1) / 2;
        for m in 2..=num.isqrt() {
            v -= sum_phi(num / m, cache);
        }
        for d in 1..=(num / (num.isqrt() + 1)) {
            v -= ((num / d) - (num / (d + 1))) * sum_phi(d, cache);
        }

        cache.insert(num, v);
        v
    }
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0072_8() {
        assert_eq!(compute(8), 21);
    }
    #[test]
    fn p0072_1000000() {
        assert_eq!(compute(1_000_000), 303963552391);
    }
}
