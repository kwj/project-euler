// Project Euler: Problem 56

euler::run_solver!(56);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> usize {
    use num_bigint::BigUint;
    use std::cmp;

    let mut ans: usize = 0;

    for a in (1_u32..100).rev() {
        // assume that x = 10 * n
        // x^y = (10 * n)^y = 10^y * n^y, so sum(digits(x^y)) = sum(digits(n^y))
        // we can skip to check multiples of ten in this problem.
        if a % 10 == 0 {
            continue;
        }

        let a_big = BigUint::from(a);
        for b in (1..100).rev() {
            let p = a_big.pow(b);
            if BigUint::to_str_radix(&p, 10).len() * 9 < ans {
                break;
            }
            ans = cmp::max(ans, p.to_radix_le(10).into_iter().map(usize::from).sum());
        }
    }

    ans
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0056() {
        assert_eq!(compute(), 972);
    }
}
