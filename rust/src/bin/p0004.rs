// Project Euler: Problem 4

euler::run_solver!(4);

fn solve() -> String {
    compute(3).to_string()
}

fn compute(ndigit: u32) -> u64 {
    debug_assert!(ndigit > 0);

    let n_upper = 10_u64.pow(ndigit) - 1;
    let n_lower = if ndigit > 1 {
        10_u64.pow(ndigit - 1)
    } else {
        0
    };
    let blk_width = 10_u64.pow(ndigit * 2 - 2);

    // descending order - (100, 999, 990000, 999999), (100, 999, 980000, 989999), ...
    let mut blocks = ((n_lower * n_lower)..=((n_upper * n_upper) / blk_width * blk_width))
        .rev()
        .step_by(blk_width as usize)
        .map(|x| (n_lower, n_upper, x, x + blk_width - 1));

    if let Some(v) = blocks.find_map(max_palindrome_number) {
        v
    } else {
        unreachable!()
    }
}

fn max_palindrome_number(
    (n_lower, n_upper, blk_lower, blk_upper): (u64, u64, u64, u64),
) -> Option<u64> {
    use euler::math;
    use std::cmp;

    let mut result: Vec<u64> = Vec::new();
    for x in (n_lower..=n_upper).rev() {
        if x * x < blk_lower {
            break;
        }
        let y_upper = if let Some(v) = blk_upper.checked_div(x) {
            cmp::min(v, x)
        } else {
            x
        };
        for y in (n_lower..=y_upper).rev() {
            let tmp = x * y;
            if tmp < blk_lower {
                break;
            }
            if math::is_palindrome(tmp, 10) {
                result.push(tmp);
            }
        }
    }
    result.into_iter().max()
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
