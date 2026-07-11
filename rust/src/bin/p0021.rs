// Project Euler: Problem 21

euler::run_solver!(21);

fn solve() -> String {
    compute(10_000).to_string()
}

fn compute(limit: usize) -> u64 {
    use euler::math;

    debug_assert!(limit > 1);

    let d_tbl = math::aliquot_sum_tbl(limit - 1);

    (1..limit)
        .filter(|x| *x as u64 > d_tbl[*x] && d_tbl[d_tbl[*x] as usize] == *x as u64)
        .map(|x| x as u64 + d_tbl[x])
        .sum()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0021_10000() {
        assert_eq!(compute(10_000), 31626);
    }
}
