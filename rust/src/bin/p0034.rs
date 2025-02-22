// Project Euler: Problem 34

/*
  the maximum n-digits number is n * 9!

     10 ** (n-1) <= n * 9!
  -> n - 1 <= log10(n * 9!)
  -> n - 1 <= log10(n) + log10(9!)
  -> n - 1 <= log10(n) + 5.559
  -> n <= log10(n) + 6.559

    >>> math.log10(5)
    0.6989700043360189
    >>> math.log10(6)
    0.7781512503836436
    >>> math.log10(7)
    0.8450980400142568
    >>> math.log10(8)
    0.9030899869919435

  so, 'n' is 7 or less.

  we have two approaches to solve this problem. One is to search from left hand side,
  and the other is to search from right hand side.

  1) search from LHS
    9! * 7 = 362880 * 7 = 2540160
    if the first digit is '2' on 7-digits number, the maximum number of the remaing 6-digits is 999999 (6 * 9! = 2177280).
    so, 2nd-digit is 0 or 1. if 2nd-digit is 1, the maximum number 2! + 1! + 5*9! = 2 + 1 + 1814400 = 1814403.
    This is a contradiction, so the answer I look for is 1_999_999 or less.

    assume that a 7-digits number '1 d_{1} .. d{6}', any d_{i} >= 5. it becomes 1! + sum(d_{i}!) mod 10 = 1. it's a contradiction.
    so, at least one d_{i} is equal or less than 4. 1! + 4! + 5 * 9! = 1814425. 1! + 8! + 4! + 4 * 9! = 1491865.

    # An example in Python
    def search_from_lhs():
        limit = 1_491_865
        memo_tbl = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880] + [0] * (limit + 1 - 10)
        acc = 0
        for n in range(10, limit + 1):
            memo_tbl[n] = memo_tbl[n // 10] + memo_tbl[n % 10]
            if n == memo_tbl[n]:
               acc += n

        return acc

  2) search from RHS
    We search from combinations of numbers.
*/

euler::run_solver!(34);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    use euler::math;
    use itertools::Itertools;

    let fact_tbl: Vec<i64> = vec![1, 1, 2, 6, 24, 120, 720, 5_040, 40_320, 362_880];
    let mut acc = 0_i64;

    for ndigits in 2_usize..8 {
        for v in (0_usize..10).combinations_with_replacement(ndigits) {
            let n = v.iter().map(|x| fact_tbl[*x]).sum::<i64>();
            let mut tmp: Vec<_> = math::digits(n).into_iter().map(|x| x as usize).collect();
            tmp.sort_unstable();
            if tmp == v {
                acc += n;
            }
        }
    }

    acc
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0034() {
        assert_eq!(compute(), 40_730);
    }
}
