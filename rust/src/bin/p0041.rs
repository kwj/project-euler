// Project Euler: Problem 41

euler::run_solver!(41);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> u64 {
    if let Some(n) = find_number(&[7, 4]) {
        n
    } else {
        unreachable!()
    }
}

fn find_number(n_digits_lst: &[u64]) -> Option<u64> {
    use euler::math::{self, primes};
    use itertools::Itertools;

    n_digits_lst
        .iter()
        .flat_map(|k| (1..=*k).rev().permutations(*k as usize))
        .find_map(|lst| {
            let n = lst.into_iter().fold(0, |acc, x| 10 * acc + x);
            if math::is_pandigital_nz(n) && primes::is_prime(n) {
                Some(n)
            } else {
                None
            }
        })
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0041() {
        assert_eq!(compute(), 7652413);
    }
}
