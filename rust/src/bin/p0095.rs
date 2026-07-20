// Project Euler: Problem 95

euler::run_solver!(95);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> usize {
    use euler::math;

    let limit: usize = 1_000_000;

    // spd: sum of proper divisors
    let spd_tbl = math::aliquot_sum_tbl(limit);

    // chain_tbl
    //   0: unconfirmed
    //  -1: not amicable chain
    //   n: length of amicable chain
    let mut chain_tbl = vec![0_i64; limit + 1];

    let mut chain: Vec<usize> = Vec::new();
    let mut max_length: usize = usize::MIN;

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
            let i = chain.iter().position(|&x| x == pos).unwrap();
            let (c1, c2) = chain.split_at(i);
            let length = c2.len();

            update_chain_tbl(&mut chain_tbl, c1, -1);
            update_chain_tbl(&mut chain_tbl, c2, length as i64);
            max_length = max_length.max(length);
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
