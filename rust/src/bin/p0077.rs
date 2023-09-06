// Project Euler: Problem 77

euler::run_solver!(77);

fn solve() -> String {
    compute(5_000).to_string()
}

fn compute(thr: i64) -> usize {
    use euler::math::primes;

    let mut prime = 0;
    let mut p_lst: Vec<i64> = Vec::new();

    loop {
        prime = primes::next_prime(prime);
        p_lst.push(prime);
        let mut tbl: Vec<i64> = vec![0; p_lst.len() + 1];
        tbl[0] = 1;
        for i in p_lst.iter() {
            for j in *i..(tbl.len() as i64) {
                tbl[j as usize] += tbl[(j - *i) as usize];
            }
        }

        if *tbl.last().unwrap() > thr {
            break;
        }
    }
    p_lst.len()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0076_4() {
        assert_eq!(compute(4), 10);
    }

    #[test]
    fn p0077_5000() {
        assert_eq!(compute(5_000), 71);
    }
}
