// Project Euler: Problem 4

euler::run_solver!(4);

fn solve() -> String {
    compute(3).to_string()
}

fn compute(ndigit: u32) -> u64 {
    // u64::MAX = 18446744073709551615 (20-digits number)
    // So, limit the maximum number of digits to less than 10.
    debug_assert!(ndigit > 0 && ndigit < 10);

    let n_upper = 10_u64.pow(ndigit) - 1;
    let n_lower = if ndigit > 1 {
        10_u64.pow(ndigit - 1)
    } else {
        0
    };

    let is_product_of_twos = |p: u64| -> bool {
        if p == 0 {
            return true;
        }

        let step = ((p & 1) + 1) as usize;
        let x_upper = |x: u64| {
            if x & 1 == 0 && p & 1 == 1 { x - 1 } else { x }
        };

        for x in (p.isqrt()..=x_upper(n_upper.min(p / n_lower.max(1))))
            .rev()
            .step_by(step)
        {
            if p.is_multiple_of(x) {
                let y = p / x;
                if y >= n_lower && y <= n_upper {
                    return true;
                }
            }
        }

        false
    };

    let it1 = (n_lower..=n_upper).rev().filter_map(palindrome_even_digits);
    let it2 = (n_lower..=n_upper).rev().map(palindrome_odd_digits);

    if let Some(ans) = it1.chain(it2).find(|&p| is_product_of_twos(p)) {
        ans
    } else {
        unreachable!()
    }
}

// xyz -> xyzzyx
fn palindrome_even_digits(mut n: u64) -> Option<u64> {
    if n == 0 {
        None
    } else {
        let mut x = n;
        let mut r;
        while n > 0 {
            (n, r) = (n / 10, n % 10);
            x = 10 * x + r;
        }
        Some(x)
    }
}

// xyz -> xyzyx
fn palindrome_odd_digits(mut n: u64) -> u64 {
    let mut x = n;
    let mut r;
    n /= 10;
    while n > 0 {
        (n, r) = (n / 10, n % 10);
        x = 10 * x + r;
    }
    x
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0004_one_digit() {
        assert_eq!(compute(1), 9);
    }

    #[test]
    fn p0004_two_digit() {
        assert_eq!(compute(2), 9009);
    }

    #[test]
    fn p0004_three_digit() {
        assert_eq!(compute(3), 906609);
    }
}
