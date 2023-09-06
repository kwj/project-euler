// Project Euler: Problem 9

/*
 a = k(m^2 - n^2), b = k * 2mn, c = k(m^2 + n^2)  [m>n>0, gcd(m,n)=1, m+n is odd, k>0]

 abc = k^3 * (m^4 - n^4) * 2mn
 a + b + c = k * 2m(m+n) = 1000

 -> 'k' and 'm' are divisors to 500 (= 1000/2).
    'm+n' is a divisor to 500/m.
    m(m+n) <= 500 --> m <= isqrt(500), m+n <= 500/m
*/

euler::run_solver!(9);

fn solve() -> String {
    compute(1_000).to_string()
}

fn compute(perim: i64) -> i64 {
    use euler::math;

    for m in 2..=math::isqrt(perim / 2) {
        if (perim / 2) % m != 0 {
            continue;
        }
        // assume that x = m + n where x is odd number
        let mut x = m + 1 + (m % 2);
        while x < 2 * m && x <= (perim / 2) / m {
            if math::gcd(m, x) == 1 && (perim / 2) / m % x == 0 {
                let k = (perim / 2) / m / x;
                let n = x - m;
                return k.pow(3) * (m.pow(4) - n.pow(4)) * 2 * m * n;
            }
            x += 2;
        }
    }

    // Not reached on this problem
    unreachable!();
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0009_perim_12() {
        assert_eq!(compute(3 + 4 + 5), 60);
    }

    #[test]
    fn p0009_perim_36() {
        assert_eq!(compute(9 + 12 + 15), 1620);
    }

    #[test]
    fn p0009_perim_1000() {
        assert_eq!(compute(1000), 31875000);
    }
}
