// Project Euler: Problem 69

/*
 https://en.wikipedia.org/wiki/Euler%27s_totient_function

   n/phi(n) = n / n(1-1/p{1})(1-1/p{2})...(1-1/p{r})
            = p{1}p{2}...p{r} / (p{1}-1)(p{2}-1)...(p{r}-1)
                   (p{i} is prime number)

 the above show that value of n/phi(n) depends on the prime factors of 'n'.

 generally, 1 < m/(m-1) and m/(m-1) > n/(n-1) [m<n].

 so I'll find the maximum 'k' which satisfies follwing condition.

   p{1} * p{2} * ... * p{k-1} * p{k} <= 1_000_000
      [p{i} is prime number: 2, 3, 5, 7, ...]

 the answer 'n' is p{1} * p{2} * ... * p{k-1} * p{k}.
*/

euler::run_solver!(69);

fn solve() -> String {
    compute(1_000_000).to_string()
}

fn compute(limit: i64) -> i64 {
    use euler::math::primes;

    let mut acc = 1_i64;
    let mut p = 1_i64;
    loop {
        p = primes::next_prime(p);
        if acc * p > limit {
            break;
        }
        acc *= p;
    }
    acc
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0069_10() {
        assert_eq!(compute(10), 6);
    }

    #[test]
    fn p0069_1000000() {
        assert_eq!(compute(1_000_000), 510510);
    }
}
