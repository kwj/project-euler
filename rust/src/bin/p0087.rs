// Project Euler: Problem 87

/*
  >>> 50000000 ** (1/2)
  7071.067811865475
  >>> 50000000 ** (1/3)
  368.40314986403854
  >>> 50000000 ** (1/4)
  84.08964152537145
*/

euler::run_solver!(87);

fn solve() -> String {
    compute(50_000_000).to_string()
}

fn compute(thr: i64) -> usize {
    use euler::math::{self, primes};
    use std::collections::HashSet;

    let mut res: HashSet<i64> = HashSet::new();
    let p_lst = primes::primes(1, math::isqrt(thr));

    for z4 in p_lst.iter().map(|n| (*n).pow(4)).take_while(|n| *n <= thr) {
        for y3 in p_lst.iter().map(|n| (*n).pow(3)).take_while(|n| *n <= thr) {
            let tmp = z4 + y3;
            if tmp >= thr {
                break;
            }
            for x2 in p_lst.iter().map(|n| (*n).pow(2)) {
                if tmp + x2 < thr {
                    res.insert(tmp + x2);
                }
            }
        }
    }
    res.len()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0087_50() {
        assert_eq!(compute(50), 4);
    }

    #[test]
    fn p0087_50000000() {
        assert_eq!(compute(50_000_000), 1097343);
    }
}
