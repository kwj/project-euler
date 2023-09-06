// Project Euler: Problem 12

/*
- Assume that f(n) returns number of divisors of 'n'.
    f(a*b) = f(a) * f(b) when 'a' and 'b' are relatively prime.
- Triangle number's formula is n(n + 1)/2. Besides 'n' and 'n + 1' are relatively prime.

Therefore, ...
  - 'n/2' and 'n+1' are relatively prime (when 'n' is even)
  - 'n' and '(n+1)/2' are relatively prime (when 'n' is odd)
*/

euler::run_solver!(12);

fn solve() -> String {
    compute(500).to_string()
}

fn compute(thr: i64) -> i64 {
    let mut n: i64 = 1;
    loop {
        if num_of_divs(n) * num_of_divs((n + 1) / 2) > thr {
            break;
        };
        n += 1;
        if num_of_divs(n / 2) * num_of_divs(n + 1) > thr {
            break;
        }
        n += 1;
    }
    (n * (n + 1)) / 2
}

fn num_of_divs(n: i64) -> i64 {
    use euler::math;

    if n == 1 {
        1
    } else {
        math::factorize(n)
            .into_iter()
            .map(|(_, x)| x + 1)
            .product::<i64>()
    }
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0012_threshold_5() {
        assert_eq!(compute(5), 28);
    }

    #[test]
    fn p0012_threshold_500() {
        assert_eq!(compute(500), 76576500);
    }
}
