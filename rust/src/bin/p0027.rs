// Project Euler: Problem 27

/*
  n^2 + an + b,  where abs(a) < 1000 and abs(b) < 1000

  when 'n' = 0:
    '0 + 0 + b' = 'b' must be a prime number. so, 2 < 'b' < 1000.
    ('b' must not be 2 since value of the expression becomes even when 'n' is an even number)
  when 'n' = 1:
    '1 + a + b' must be a prime number. write this prime number is 'x', then 'a' = 'x' - 'b' - 1.
    abs('x' - b - 1) < 1000 and 2 < 'b' < 1000 ===> 0 < 'x' < 2000
  when 'n' is a odd number:
    'n^2 + b' is a even number. so 'a' must be a odd number.
*/

use euler::math::primes;

euler::run_solver!(27);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    let p_lst = primes::primes(1, 2_000);
    let mut max_len = 0_i64;
    let mut max_tpl: (i64, i64) = (0, 0);

    for &b in p_lst[1..].iter().filter(|&x| *x < 1_000) {
        for a in p_lst
            .iter()
            .map(|&x| x - b - 1)
            .filter(|&x| x.abs() < 1_000)
        {
            let length = count_consec_times(a, b);
            if length > max_len {
                max_len = length;
                max_tpl = (a, b);
            }
        }
    }

    max_tpl.0 * max_tpl.1
}

fn count_consec_times(a: i64, b: i64) -> i64 {
    let mut n = 0_i64;

    while primes::is_prime(n * n + a * n + b) {
        n += 1;
    }

    n
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0027() {
        assert_eq!(compute(), -59231);
    }
}
