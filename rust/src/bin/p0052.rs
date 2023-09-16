// Project Euler: Problem 52

euler::run_solver!(52);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    for exp in 6_u32.. {
        for n in 10_i64.pow(exp - 1)..=(10_i64.pow(exp) / 6) {
            if check_num(n) {
                return n;
            }
        }
    }

    // Not reached on this problem
    unreachable!();
}

fn check_num(num: i64) -> bool {
    use euler::math;

    fn make_key(n: i64) -> Vec<i64> {
        let mut key = math::digits(n);
        key.sort();
        key
    }

    let key = make_key(num);
    for mult in 2..=6 {
        if key != make_key(num * mult) {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0052() {
        assert_eq!(compute(), 142857);
    }
}
