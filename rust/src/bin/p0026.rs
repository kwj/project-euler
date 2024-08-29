// Project Euler: Problem 26

euler::run_solver!(26);

fn solve() -> String {
    compute(1_000).to_string()
}

fn compute(upper: i64) -> i64 {
    let mut max_length = 0_i64;
    let mut ans = 0_i64;

    for d in ((upper / 2)..upper).rev() {
        if d <= max_length {
            break;
        }
        if let Some(tpl) = find_repetend_length(d) {
            if tpl.0 > max_length {
                (max_length, ans) = tpl;
            }
        }
    }

    ans
}

// preprocessing
fn pp(mut n: i64) -> i64 {
    while n % 2 == 0 {
        n /= 2;
    }
    while n % 5 == 0 {
        n /= 5;
    }

    n
}

fn find_repetend_length(mut n: i64) -> Option<(i64, i64)> {
    use euler::math;

    // This function is not strictly correct Carmichael function
    // because the function assumes that the argument is not a multiple of 2.
    fn carmichael(n: i64) -> i64 {
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

    // Not reached on this problem
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
