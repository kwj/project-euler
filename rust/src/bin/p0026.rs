// Project Euler: Problem 26

euler::run_solver!(26);

fn solve() -> String {
    compute(1_000).to_string()
}

fn compute(upper: u64) -> u64 {
    debug_assert!(upper > 1);

    let mut max_length = 0_u64;
    let mut ans = 0_u64;

    for d in ((upper / 2)..upper).rev() {
        if d <= max_length {
            break;
        }
        if let Some(tpl @ (length, _)) = find_repetend_length(d)
            && length > max_length
        {
            (max_length, ans) = tpl;
        }
    }

    ans
}

// preprocessing
fn pp(mut n: u64) -> u64 {
    while n.is_multiple_of(2) {
        n /= 2;
    }
    while n.is_multiple_of(5) {
        n /= 5;
    }

    n
}

fn find_repetend_length(mut n: u64) -> Option<(u64, u64)> {
    use euler::math;

    // This function is not strictly correct Carmichael function
    // because the function assumes that the argument is not a multiple of 2.
    fn carmichael(n: u64) -> u64 {
        math::factorize(n)
            .into_iter()
            .map(|(b, e)| (b - 1) * b.pow(e - 1))
            .reduce(math::lcm)
            .unwrap()
    }

    n = pp(n);
    if n == 1 {
        return None;
    }
    for k in math::divisors(carmichael(n)) {
        if math::powmod(10, k, n) == 1 {
            return Some((k, n));
        }
    }

    unreachable!();
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0026_upper_10() {
        assert_eq!(compute(10), 7);
    }

    #[test]
    fn p0026_upper_300() {
        assert_eq!(compute(300), 289);
    }

    #[test]
    fn p0026_upper_1000() {
        assert_eq!(compute(1_000), 983);
    }
}
