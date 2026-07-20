// Project Euler: Problem 29

// Note that use `for` loops for clarity in this solution

euler::run_solver!(29);

fn solve() -> String {
    compute(100).to_string()
}

fn compute(upper: usize) -> u64 {
    use euler::math;

    debug_assert!(upper >= 2);

    let dupctr_tbl = make_dupctr_tbl(upper);
    let base_limit = upper.isqrt();
    let mut skip_flag = vec![false; base_limit + 1];
    let mut ans = (upper - 1).pow(2) as u64;

    for b in 2..=base_limit {
        if skip_flag[b] {
            continue;
        }
        for (e, &elm) in dupctr_tbl
            .iter()
            .enumerate()
            .take(math::get_max_exp(upper as u64, b as u64) as usize + 1)
            .skip(2)
        {
            ans -= elm;
            let tmp = b.pow(e as u32);
            if tmp <= base_limit {
                skip_flag[tmp] = true;
            }
        }
    }

    ans
}

fn make_dupctr_tbl(upper: usize) -> Vec<u64> {
    use euler::math;

    let max_exp = math::get_max_exp(upper as u64, 2) as usize;
    let mut dupctr_tbl = vec![0_u64; max_exp + 1];

    for (x, elm) in dupctr_tbl.iter_mut().enumerate().take(max_exp + 1).skip(2) {
        let mut dups = vec![0_u64; upper + 1];
        for y in 1..x {
            let k = math::lcm(x as u64, y as u64) as usize / x;
            (k.max(2)..=(upper * y / x))
                .step_by(k)
                .for_each(|idx| dups[idx] = 1);
        }
        *elm = dups.iter().sum();
    }

    dupctr_tbl
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0029_upper_5() {
        assert_eq!(compute(5), 15);
    }

    #[test]
    fn p0029_upper_100() {
        assert_eq!(compute(100), 9183);
    }
}
