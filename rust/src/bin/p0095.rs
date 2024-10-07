// Project Euler: Problem 95

euler::run_solver!(95);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> usize {
    use euler::math;
    use std::cmp;

    let limit: usize = 1_000_000;

    // spd: sum of proper divisors
    let spd_tbl = math::aliquot_sum_tbl(limit);

    let mut chain_tbl = vec![0_i64; limit + 1];
    let mut chain: Vec<usize> = Vec::new();
    let mut max_length: usize = 0;

    for mut pos in 2..=limit {
        chain.clear();
        while chain_tbl[pos] == 0 {
            chain.push(pos);
            pos = spd_tbl[pos] as usize;
            if pos <= 1 || pos > limit || chain.contains(&pos) {
                break;
            }
        }

        if pos <= 1 || pos > limit || chain_tbl[pos] != 0 {
            update_chain_tbl(&mut chain_tbl, &chain, -1);
        } else {
            let mut i: usize = 0;
            while pos != chain[i] {
                i += 1;
            }
            let length = chain.len() - i + 1;
            update_chain_tbl(&mut chain_tbl, &chain[..i], -1);
            update_chain_tbl(&mut chain_tbl, &chain[i..], length as i64);
            max_length = cmp::max(max_length, length);
        }
    }

    chain_tbl
        .into_iter()
        .position(|x| x == max_length as i64)
        .unwrap()
}

fn update_chain_tbl(tbl: &mut [i64], elts: &[usize], v: i64) {
    for &idx in elts {
        tbl[idx] = v;
    }
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0095() {
        assert_eq!(compute(), 14316);
    }
}
