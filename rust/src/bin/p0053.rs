// Project Euler: Problem 53

euler::run_solver!(53);

fn solve() -> String {
    compute(100, 1_000_000).to_string()
}

fn compute(max_n: u64, thr: u64) -> u64 {
    debug_assert!(max_n > 0);

    // find the first central binomial coefficient C(2k, k) which is larger than 'thr'.
    let mut c: u64 = 2;
    let mut x: u64 = 1;
    while c <= thr {
        x += 1;
        c = c * (4 * x - 2) / x;
    }

    // the start position 'c' = C(n, r) is C(2k-1, k-1). And then set x = (n - r + 1).
    let mut r: u64 = x - 1;
    x += 1;
    c /= 2;

    let mut ans: u64 = 0;
    for n in (r + x - 1)..=max_n {
        while c > thr {
            c = c * r / x;
            r -= 1;
            x += 1;
        }
        ans += n - 2 * r - 1;

        // go to next row (down-left)
        c = c * (n + 1) / x;
        x += 1;
    }

    ans
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0053_23_1000000() {
        assert_eq!(compute(23, 1_000_000), 4);
    }

    #[test]
    fn p0053_100_1000000() {
        assert_eq!(compute(100, 1_000_000), 4075);
    }
}
