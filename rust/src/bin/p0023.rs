// Project Euler: Problem 23

euler::run_solver!(23);

fn solve() -> String {
    compute(28_123).to_string()
}

fn compute(limit: usize) -> i64 {
    use euler::math;

    let mut d_tbl = math::get_sigma_tbl(1, limit);
    for (idx, elm) in d_tbl.iter_mut().enumerate().skip(1) {
        *elm -= idx as i64
    }

    let abndnt_flag: Vec<bool> = (0..=limit).map(|i| (i as i64) < d_tbl[i]).collect();
    let mut abndnt_lst: Vec<usize> = Vec::new();
    let mut acc: usize = 0;
    for i in 1..=limit {
        if i % 2 == 0 && abndnt_flag[i / 2] {
            abndnt_lst.push(i / 2);
        }
        if abndnt_lst.iter().any(|x| abndnt_flag[i - *x]) {
            continue;
        }
        acc += i;
    }
    acc as i64
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0023_28123() {
        assert_eq!(compute(28_123), 4179871);
    }
}
