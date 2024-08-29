// Project Euler: Problem 47

euler::run_solver!(47);

fn solve() -> String {
    compute(4).to_string()
}

fn compute(nfactors: i64) -> i64 {
    use euler::math;

    debug_assert!(nfactors > 1);

    let mut cnt = 0;

    // '6' is the smallest product of two distinct prime factors
    for x in 6_i64.. {
        if math::factorize(x).len() as i64 != nfactors {
            cnt = 0;
        } else if cnt == nfactors - 1 {
            return x - (nfactors - 1);
        } else {
            cnt += 1;
        }
    }

    // Not reached on this problem
    unreachable!();
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0047_2() {
        assert_eq!(compute(2), 14);
    }

    #[test]
    fn p0047_3() {
        assert_eq!(compute(3), 644);
    }

    #[test]
    fn p0047_4() {
        assert_eq!(compute(4), 134043);
    }
}
