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
    let p_lst: Vec<i64> = primes::primes(1, 2_000)
        .into_iter()
        .map(|x| i64::try_from(x).unwrap())
        .collect();

    let (a, b) = p_lst
        .iter()
        .skip(1)
        .filter(|&&p| p < 1_000)
        .flat_map(|&b| {
            p_lst.iter().filter_map(move |&p| {
                let a = p - b - 1;
                if a < 1_000 { Some((a, b)) } else { None }
            })
        })
        .max_by_key(|&(a, b)| count_consec_times(a, b))
        .unwrap();

    a * b
}

fn count_consec_times(a: i64, b: i64) -> u64 {
    let mut n = 0_i64;

    loop {
        let p = n * n + a * n + b;
        if p > 0 && primes::is_prime(p as u64) {
            n += 1;
        } else {
            break;
        }
    }

    u64::try_from(n).unwrap()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0027() {
        assert_eq!(compute(), -59231);
    }
}
