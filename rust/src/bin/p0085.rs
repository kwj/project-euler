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

fn compute(target: u64) -> u64 {
    let new_target = target * 4;
    let mut min_diff = u64::MAX;
    let mut ans: u64 = 0;

    for m in 1_u64.. {
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

fn get_diff(m: u64, target: u64) -> Option<(u64, u64)> {
    let lhs = |m: u64, n: u64| m * (m + 1) * n * (n + 1);

    let mut n = (target / (m * (m + 1))).isqrt() - 1;
    let mut x1 = 0_u64;
    let mut x2 = lhs(m, n);

    while x2 < target {
        n += 1;
        x1 = x2;
        x2 = lhs(m, n);
    }
    if m >= n {
        return None;
    }

    let d1 = target - x1;
    let d2 = x2 - target;
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
