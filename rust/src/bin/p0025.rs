// Project Euler: Problem 25

euler::run_solver!(25);

fn solve() -> String {
    compute(1_000).to_string()
}

fn compute(n_digits: u32) -> i64 {
    use num_bigint::BigUint;

    let bound = BigUint::from(10_u32).pow(n_digits - 1);
    let mut f1 = BigUint::from(1_u32);
    let mut f2 = BigUint::from(1_u32);
    let mut idx = 2_i64;
    while f2 < bound {
        (f2, f1) = (f1 + &f2, f2);
        idx += 1;
    }
    idx
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0025_ndigits_3() {
        assert_eq!(compute(3), 12);
    }

    #[test]
    fn p0025_ndigits_1000() {
        assert_eq!(compute(1_000), 4782);
    }
}
