// Project Euler: Problem 21

euler::run_solver!(21);

fn solve() -> String {
    compute(10_000).to_string()
}

fn compute(limit: usize) -> i64 {
    use euler::math;

    let mut d_tbl = math::get_sigma_tbl(1, limit - 1);
    for (idx, elm) in d_tbl.iter_mut().enumerate().skip(1) {
        *elm -= idx as i64
    }

    (2..limit)
        .filter(|&x| x as i64 > d_tbl[x] && d_tbl[d_tbl[x] as usize] == x as i64)
        .map(|x| x as i64 + d_tbl[x])
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
