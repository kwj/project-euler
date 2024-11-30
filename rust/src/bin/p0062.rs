// Project Euler: Problem 62

euler::run_solver!(62);

fn solve() -> String {
    compute(5).to_string()
}

fn compute(n_perms: usize) -> i64 {
    use euler::math;
    use std::collections::HashMap;

    fn make_key(n: i64) -> i64 {
        let mut tmp = math::digits(n);
        tmp.sort_unstable();
        math::undigits(&tmp)
    }

    debug_assert!(n_perms > 1);

    let mut tbl: HashMap<i64, Vec<i64>> = HashMap::new();
    for n in 1_i64.. {
        let cube = n.pow(3);
        let key = make_key(cube);
        tbl.entry(key).or_default().push(cube);
        let tmp = tbl.get(&key).unwrap();
        if tmp.len() == n_perms {
            return tmp[0];
        }
    }

    unreachable!();
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0062_3() {
        assert_eq!(compute(3), 41063625);
    }

    #[test]
    fn p0062_5() {
        assert_eq!(compute(5), 127035954683);
    }
}
