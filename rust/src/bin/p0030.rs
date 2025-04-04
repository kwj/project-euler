// Project Euler: Problem 30

/*
  (1) 10 ** (n-1) <= n-digits number < 10 ** n
  (2) assume that x is sum of the fifth power of each digit on n-digits number
        n * (1**5) <= x <= n * (9**5) = n * 54049

     when n=6:
       6 * 54049 = 324294
     when n=7
       7 * 54049 = 378343 < 10 ** (7-1) = 1000000 (minimum 7-digits number)
       --> contradiction

  It's clear that 'x' is not a single digit number.
  We need to search 'x' in the follwing range:
    10 <= 'x' <= 354294 = 6 * (9 ** 5)

  We have two approaches to solve this problem. One is to search from left hand side,
  and the other is to search from right hand side.

  1) search from LHS

    # an example in Python
    def search_from_lhs():
        limit = 354_294
        memo_tbl = [0, 1, 32, 243, 1024, 3125, 7776, 16807, 32768, 59049] + [0] * (limit + 1 - 10)
        acc = 0
        for n in range(10, limit + 1):
            memo_tbl[n] = memo_tbl[n // 10] + memo_tbl[n % 10]
            if n == memo_tbl[n]:
                acc += n

        return acc

  2) search from RHS
    We search from combinations of numbers.
*/

euler::run_solver!(30);

fn solve() -> String {
    compute(5).to_string()
}

fn compute(exp: u32) -> i64 {
    use euler::math;
    use itertools::Itertools;

    debug_assert!(exp > 1);

    let pow_tbl: Vec<i64> = (0_i64..=9).map(|x| x.pow(exp)).collect();
    let mut acc = 0_i64;

    // The combinations generated by combinations_with_replacement() are expected
    // to be output in lexicographic order according to the order of the input iterables.
    for n_digits in 2..=get_max_ndigits(exp) {
        for tpl in (0_i64..=9).combinations_with_replacement(n_digits) {
            let tmp: i64 = tpl.iter().map(|x| pow_tbl[*x as usize]).sum();
            let mut key = math::digits(tmp);
            key.sort_unstable();
            if key == tpl {
                acc += tmp;
            }
        }
    }

    acc
}

fn get_max_ndigits(exp: u32) -> usize {
    let k = 9_usize.pow(exp);
    let mut x = 2_u32;

    while (x as usize) * k > 10_usize.pow(x - 1) {
        x += 1;
    }

    x as usize - 1
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0030_exp_4() {
        assert_eq!(compute(4), 19316);
    }

    #[test]
    fn p0030_exp_5() {
        assert_eq!(compute(5), 443839);
    }
}
