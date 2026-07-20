// Project Euler: Problem 77

euler::run_solver!(77);

fn solve() -> String {
    compute(5_000).to_string()
}

fn compute(thr: u64) -> usize {
    use euler::math::primes;

    let mut prime = 0;
    let mut p_lst: Vec<usize> = Vec::new();
    let mut tbl: Vec<u64> = vec![0; 1];

    loop {
        let target = tbl.len();
        tbl.fill(0);
        tbl.push(0);
        tbl[0] = 1;

        prime = primes::next_prime(prime);
        p_lst.push(prime as usize);
        p_lst
            .iter()
            .flat_map(|&i| (i..=target).map(move |j| (i, j)))
            .for_each(|(i, j)| tbl[j] += tbl[j - i]);

        if tbl[target] > thr {
            return target;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0077_4() {
        assert_eq!(compute(4), 10);
    }

    #[test]
    fn p0077_5000() {
        assert_eq!(compute(5_000), 71);
    }
}
