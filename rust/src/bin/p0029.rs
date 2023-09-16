// Project Euler: Problem 29

euler::run_solver!(29);

fn solve() -> String {
    compute(100).to_string()
}

fn compute(upper: usize) -> i64 {
    use euler::math;

    let dup_ctr = make_dupctr_tbl(upper);
    let base_limit = math::isqrt(upper as i64) as usize;
    let mut skip_flag = vec![false; base_limit + 1];

    let mut ans = (upper - 1).pow(2) as i64;
    for b in 2..=base_limit {
        if skip_flag[b] {
            continue;
        }
        for (e, elm) in dup_ctr
            .iter()
            .enumerate()
            .take(math::get_max_exp(upper as i64, b as i64) as usize + 1)
            .skip(2)
        {
            ans -= *elm;
            let tmp = b.pow(e as u32);
            if tmp <= base_limit {
                skip_flag[tmp] = true;
            }
        }
    }
    ans
}

fn make_dupctr_tbl(upper: usize) -> Vec<i64> {
    use euler::math;
    use std::cmp;

    let max_exp = math::get_max_exp(upper as i64, 2) as usize;
    let mut dup_ctr = vec![0_i64; max_exp + 1];

    for (x, elm) in dup_ctr.iter_mut().enumerate().take(max_exp + 1).skip(2) {
        let mut dups = vec![0_i64; upper + 1];
        for y in 1..x {
            let k = math::lcm(x as i64, y as i64) as usize / x;
            (cmp::max(k, 2)..=(upper * y / x))
                .step_by(k)
                .for_each(|idx| dups[idx] = 1);
        }
        *elm = dups.iter().sum();
    }
    dup_ctr
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0029_upper_100() {
        assert_eq!(compute(100), 9183);
    }
}
