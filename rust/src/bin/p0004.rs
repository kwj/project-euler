// Project Euler: Problem 4

euler::run_solver!(4);

fn solve() -> String {
    compute(3).to_string()
}

fn compute(ndigit: u32) -> i64 {
    use euler::math;
    use std::cmp;

    debug_assert!(ndigit > 0);

    let n_upper = 10_i64.pow(ndigit) - 1;
    let n_lower = 10_i64.pow(ndigit - 1);
    let blk_upper_limit = 10_i64.pow(ndigit * 2);
    let blk_lower_limit = if ndigit > 1 {
        10_i64.pow((ndigit - 1) * 2)
    } else {
        0
    };
    let blk_width = 10_i64.pow(ndigit * 2 - 2);
    let mut answer: Vec<i64> = Vec::new();

    for (blk_lower, blk_upper) in ((blk_lower_limit + blk_width)..=blk_upper_limit)
        .rev()
        .step_by(blk_width as usize)
        .map(|x| (x - blk_width, x - 1))
    {
        for x in (n_lower..=n_upper).rev() {
            if x * x < blk_lower {
                break;
            }
            for y in (n_lower..=cmp::min(blk_upper / x, x)).rev() {
                let tmp = x * y;
                if tmp < blk_lower {
                    break;
                }
                if math::is_palindrome(tmp, 10) {
                    answer.push(tmp);
                }
            }
        }
        if !answer.is_empty() {
            return answer.into_iter().max().unwrap();
        }
    }

    unreachable!();
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0004_two_digit() {
        assert_eq!(compute(2), 9009);
    }

    #[test]
    fn p0004_three_digit() {
        assert_eq!(compute(3), 906609);
    }
}
