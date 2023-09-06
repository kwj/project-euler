// Project Euler: Problem 44

/*
  P(d) = P(k) - P(j) <==> d(3d-1) = k(3k-1) - j(3j-1) = (k-j)(3(k+j)-1)
    lhs: d(3d-1)
    rhs: (k-j) * (3(k+j)-1) = r1 * r2 [r1=k-j, r2=3(k+j)-1]
    0 < (k-j) < d, d % 3 == (k-j) % 3
*/

euler::run_solver!(44);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    use euler::math;

    let pent = |n: i64| n * (3 * n - 1) / 2;

    for d in 4_i64.. {
        let lhs = d * (3 * d - 1);
        for r1 in get_divisors(d) {
            let r2 = lhs / r1;
            if r2 % 3 != 2 {
                continue;
            }

            // if 'tmp' is even, k = (tmp / 2) and j = k - r1
            let tmp = (r2 + 1) / 3 + r1;
            if tmp % 2 != 0 {
                continue;
            }
            let k = tmp / 2;
            let j = k - r1;
            if math::is_pentagonal(pent(k) + pent(j)) {
                return lhs / 2;
            }
        }
    }

    // Not reached on this problem
    unreachable!();
}

// get_divisors(n) returns divisors of n(3n-1) which meet the following requirements:
//  - They are less than 'n'.
//  - They are congruent to 'n' modulo 3.
// note: 'n' and '3n-1' are relatively prime.
fn get_divisors(n: i64) -> Vec<i64> {
    use euler::math;
    use itertools::Itertools;

    math::divisors(n)
        .into_iter()
        .cartesian_product(math::divisors(3 * n - 1))
        .map(|(x, y)| x * y)
        .filter(|&x| x < n && x % 3 == n % 3)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0044() {
        assert_eq!(compute(), 5482660);
    }
}
