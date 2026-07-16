// Project Euler: Problem 37

euler::run_solver!(37);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> u64 {
    right_truncatable_primes()
        .into_iter()
        .filter(|x| check_left_truncatable(*x))
        .sum()
}

fn right_truncatable_primes() -> Vec<u64> {
    let mut primes: Vec<u64> = vec![2, 3, 5, 7];
    let mut cands: Vec<u64> = vec![2, 3, 5, 7];

    while !cands.is_empty() {
        cands = make_cands(&cands, &[1, 3, 7, 9]);
        primes.extend(cands.clone());
    }

    make_cands(&primes, &[3, 7])
}

fn make_cands(lst: &[u64], nums: &[u64]) -> Vec<u64> {
    use euler::math::primes;
    use itertools::Itertools;

    lst.iter()
        .cartesian_product(nums)
        .filter_map(|(x, y)| {
            let tmp = 10 * (*x) + (*y);
            if primes::is_prime(tmp) {
                Some(tmp)
            } else {
                None
            }
        })
        .collect()
}

fn check_left_truncatable(n: u64) -> bool {
    use euler::math::primes;

    let mut m = 10;

    loop {
        let (q, r) = (n / m, n % m);
        if q == 0 {
            return true;
        } else if !primes::is_prime(r) {
            return false;
        }

        m *= 10;
    }
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0037() {
        assert_eq!(compute(), 748317);
    }
}
