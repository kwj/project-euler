// Project Euler: Problem 88

/*
  N(k) = a1 + a2 + ... + ak = a1 * a2 * ... * ak
    N(k) must be composite numbers.
  min_N(k): minimal product-sum N(k)

  when k = 2
    sum {2,2} = prod {2,2}
  when k > 2 and {a1, a2, a3, ..., ak}
    min sum = sum {1, 1, ..., 1} = k
    --> min_N(k) >= k
  when k > 2 and {a1, a2, a3, ..., ak} = {1, ..., 1, 2, k}
    for all k>2, there exists Ak = {a1, a2, ..., ak} = {1, 1, ..., 1, 2, k}, prod Ak = sum Ak = N(k) = 2k
    --> min_N(k) <= 2k

  2 <= k <= 12000
  --> k <= min_N(k) <= 24000
*/

euler::run_solver!(88);

fn solve() -> String {
    compute(12_000).to_string()
}

fn compute(limit: usize) -> usize {
    let mut tbl = vec![limit * 2; limit + 1];
    aux(1, 0, 0, 2, &mut tbl, limit);

    tbl.remove(0);
    tbl.remove(0);
    tbl.sort();
    tbl.dedup();
    tbl.into_iter().sum()
}

fn aux(p: usize, s: usize, length: usize, num: usize, tbl: &mut [usize], limit: usize) {
    // p: product, s: sum
    let k = p - s + length;
    if k > limit {
        return;
    }
    if p < tbl[k] {
        tbl[k] = p;
    }

    let next_length = length + 1;
    for x in num..=((limit * 2) / p) {
        aux(p * x, s + x, next_length, x, tbl, limit);
    }
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0088_6() {
        assert_eq!(compute(6), 30);
    }

    #[test]
    fn p0088_12() {
        assert_eq!(compute(12), 61);
    }

    #[test]
    fn p0088_12000() {
        assert_eq!(compute(12_000), 7587457);
    }
}
