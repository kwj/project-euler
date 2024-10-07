// Project Euler: Problem 85

/*
  nCr = n! / ((n-r)! * r!)

      1  2       n-1  n
    +--+--+-- ... --+--+
   1|  |  |   ...   |  |
    +--+--+-- ... --+--+
   2|  |  |   ...   |  |
    +--+--+-- ... --+--+ num of horizontal lines = m + 1
   3|  |  |   ...   |  |
    +--+--+-- ... --+--+
    ....................
    +--+--+-- ... --+--+
   m|  |  |   ...   |  |
    +--+--+-- ... --+--+
      num of vertical lines = n + 1

  (m+1)C2 * (n+1)C2 = m(m+1)/2 * n(n+1)/2 (\approx) 2_000_000
    --> m(m+1)*n(n+1) ≈ 8_000_000
*/

euler::run_solver!(85);

fn solve() -> String {
    compute(2_000_000).to_string()
}

fn compute(target: i64) -> i64 {
    let new_target = target * 4;
    let mut min_diff = i64::MAX;
    let mut ans: i64 = 0;

    for m in 1_i64.. {
        if let Some(tpl) = get_diff(m, new_target) {
            if tpl.0 < min_diff {
                ans = m * tpl.1;
                min_diff = tpl.0;
            }
        } else {
            break;
        }
    }

    ans
}

fn get_diff(m: i64, target: i64) -> Option<(i64, i64)> {
    use euler::math;

    let lhs = |m: i64, n: i64| m * (m + 1) * n * (n + 1);

    let mut n = math::isqrt(target / (m * (m + 1))) - 1;
    while lhs(m, n) < target {
        n += 1;
    }

    if m >= n {
        return None;
    }
    let d1 = (target - lhs(m, n - 1)).abs();
    let d2 = (target - lhs(m, n)).abs();
    if d1 < d2 {
        Some((d1, n - 1))
    } else {
        Some((d2, n))
    }
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0085_18() {
        assert_eq!(compute(18), 6);
    }

    #[test]
    fn p0085_2000000() {
        assert_eq!(compute(2_000_000), 2772);
    }
}
