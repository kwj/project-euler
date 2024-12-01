// Project Euler: Problem 51

//use euler::math::primes;

euler::run_solver!(51);

fn solve() -> String {
    compute(8).to_string()
}

fn compute(f_size: i64) -> i64 {
    use euler::math::primes;

    debug_assert!(f_size >= 8);

    for exp in 3_u32.. {
        let p_lst = primes::primes(10_i64.pow(exp), 10_i64.pow(exp + 1));
        for p in p_lst {
            if is_family(p, f_size) {
                return p;
            }
        }
    }

    unreachable!();
}

fn is_family(p: i64, f_size: i64) -> bool {
    use euler::math::{self, primes};

    for n in 0..=(10 - f_size) {
        let p_digits = math::digits(p);
        let masks = euler::powerset(&euler::findall(|&x| x == n, &p_digits));
        for mask in masks {
            if mask.len() < 3 || mask.len() % 3 != 0 || mask[0] == 0 {
                continue;
            }
            let mut cnt: i64 = 1;
            let mut p_digits_tmp = p_digits.clone();
            for d in (n + 1)..=9 {
                for idx in &mask {
                    p_digits_tmp[*idx] = d;
                }
                if primes::is_prime(math::undigits(&p_digits_tmp)) {
                    cnt += 1;
                }
                if cnt == f_size {
                    return true;
                } else if (f_size - cnt) > (9 - d) {
                    // size of this prime family is less than 'f_size'
                    break;
                }
            }
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0051_size_8() {
        assert_eq!(compute(8), 121313);
    }
}
