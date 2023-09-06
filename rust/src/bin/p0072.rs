// Project Euler: Problem 72

/*
  https://mathproblems123.wordpress.com/2018/05/10/sum-of-the-euler-totient-function/
*/

use std::collections::HashMap;

euler::run_solver!(72);

fn solve() -> String {
    compute(1_000_000).to_string()
}

fn compute(limit: i64) -> i64 {
    let mut cache: HashMap<i64, i64> = HashMap::new();
    sum_phi(limit, &mut cache) - sum_phi(1, &mut cache)
}

fn sum_phi(num: i64, cache: &mut HashMap<i64, i64>) -> i64 {
    use euler::math;

    if let Some(val) = cache.get(&num) {
        *val
    } else {
        let mut v = num * (num + 1) / 2;
        for m in 2..=math::isqrt(num) {
            v -= sum_phi(num / m, cache);
        }
        for d in 1..=(num / (math::isqrt(num) + 1)) {
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
