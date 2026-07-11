// Project Euler: Problem 52

euler::run_solver!(52);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> u64 {
    for exp in 6_u32.. {
        for n in 10_u64.pow(exp - 1)..=(10_u64.pow(exp) / 6) {
            if check_num(n) {
                return n;
            }
        }
    }

    unreachable!();
}

fn check_num(num: u64) -> bool {
    use euler::math;

    fn make_key(n: u64) -> Vec<u64> {
        let mut key = math::digits(n);
        key.sort_unstable();
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
