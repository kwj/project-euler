// Project Euler: Problem 73

/*
  f(n): number of fractions a/b, where a < b, b <= n, 1/3 < a/b < 1/2
        --> sigma{i=1, ...,n}((i-1)//2 - i//3)
  g(n): number of irreducible fractions a/b, where a < b, b <= n, 1/3 < a/b < 1/2, gcd(a,b)=1

    The answer we should seek is g(12000).

  f(n) = sigma{k=1, ..., n}(g(n//k))
  -->
    g(n) = sigma{k=1, ..., n}μ(k)f(n//k)      [möbius inversion formula, μ(): möbius function]
         = sigma{k=1, ..., n}μ(k)sigma{j=1, ..., n//k}((j-1)//2 - j//3)
*/

euler::run_solver!(73);

fn solve() -> String {
    compute(12_000).to_string()
}

fn compute(limit: usize) -> i64 {
    g(limit)
}

fn g(limit: usize) -> i64 {
    let mu_tbl = make_mobius_tbl(limit);
    (1..=limit).map(|k| mu_tbl[k] * f((limit / k) as i64)).sum()
}

fn f(x: i64) -> i64 {
    (1..=x).map(|j| ((j - 1) / 2) - (j / 3)).sum()
}

fn make_mobius_tbl(limit: usize) -> Vec<i64> {
    use euler::math;

    let mut p_tbl: Vec<usize> = (0..=limit).collect();
    for i in 2..=(math::isqrt(limit as i64) as usize) {
        if p_tbl[i] == i {
            let k = i * i;
            for j in (k..=limit).step_by(i) {
                p_tbl[j] = i;
            }
            for j in (k..=limit).step_by(k) {
                p_tbl[j] = 0;
            }
        }
    }

    let mut mu_tbl = vec![0_i64; limit + 1];
    mu_tbl[1] = 1;
    for i in 2..=limit {
        if p_tbl[i] != 0 {
            mu_tbl[i] = -mu_tbl[i / p_tbl[i]];
        }
    }
    mu_tbl
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0073_8() {
        assert_eq!(compute(8), 3);
    }
    #[test]
    fn p0073_12000() {
        assert_eq!(compute(12_000), 7295372);
    }
}
