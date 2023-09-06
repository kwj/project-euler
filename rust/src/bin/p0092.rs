// Project Euler: Problem 92

/*
  9^2 = 81
  -->
        99 -> 9^2 * 2 = 162
       999 -> 9^2 * 3 = 243
      9999 -> 9^2 * 4 = 324
     99999 -> 9^2 * 5 = 405
    999999 -> 9^2 * 6 = 486
   9999999 -> 9^2 * 7 = 567


  BTW,

  This problem can also be solved by combination and permutation because
  the next chain is determined by combination of numeric digit.

  Once a combination of digit numbers is determined, the total number of
  numbers represented by the combination, i.e., the number of permutations
  of multisets, can be obtained.

    n! / (k{1}! * k{2}! * ... * k{n}!)   [where n = k{1} + k{2} + ... + k{n}]

  For exapmle, we assume that a 4-digit combination with repetition is {1, 2, 2, 3}.

  They can be considered as one group since next chain of all of these
  numbers is 18 (1^2 + 2^2 + 2^2 + 3^2). Note that the final number in
  this chain is 89.

  There are 12 numbers presented by the combination as following.

    1223, 1232, 1322, 2123, 2132, 2213,
    2231, 2312, 2321, 3122, 3212, 3221

  The value of 12 can be obtained from above permutations with repetitions formula:

    {num of digits}! / ({num of '1'}! * {num of '2}! * {num of '3'}!)
      = 4! / (1! * 2! * 1!)
      = 24 / 2
      = 12

  On the other hand, we assume that an another combination with repetition is {1, 2, 3, 3}.
  There are 12 numbers from the combination in the same way.

    1233, 1323, 1332, 2133, 2313, 2331,
    3123, 3132, 3213, 3231, 3312, 3321

  However, the chain from this combination {1, 2, 3, 3} arrives at 1.
  We can therefore ignore the combination.

  note: This method doesn't scale well, haha.

  Note:
    Number of combinations with repetition
    https://en.wikipedia.org/wiki/Combination#Number_of_combinations_with_repetition

    Permutations of multisets
    https://en.wikipedia.org/wiki/Permutation#Permutations_of_multisets
*/

euler::run_solver!(92);

fn solve() -> String {
    compute(10_000_000).to_string()
}

fn compute(limit: i64) -> usize {
    use euler::math;
    use itertools::Itertools;

    assert!(
        math::num_of_digits(limit, 10) == math::num_of_digits(limit - 1, 10) + 1,
        "This implementation works correctly only if the limit is a power of 10"
    );

    let n_digits = math::num_of_digits(limit - 1, 10) as usize;
    let numerator = factorial(n_digits);

    (0..10)
        .map(|i| i * i)
        .combinations_with_replacement(n_digits)
        .filter(|lst| is_group89(lst.iter().sum()))
        .map(|lst| {
            let mut acc: usize = 1;
            for (_, v) in euler::countmap(lst) {
                acc *= factorial(v);
            }
            acc
        })
        .map(|denominator| numerator / denominator)
        .sum()
}

fn factorial(n: usize) -> usize {
    if n > 1 {
        n * factorial(n - 1)
    } else {
        1
    }
}

fn is_group89(mut n: i32) -> bool {
    if n != 89 && n > 1 {
        let mut acc = 0;
        while n != 0 {
            acc += (n % 10).pow(2);
            n /= 10;
        }
        is_group89(acc)
    } else {
        n == 89
    }
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0092_10() {
        assert_eq!(compute(10), 7);
    }
    #[test]
    fn p0092_10000000() {
        assert_eq!(compute(10_000_000), 8581146);
    }
}
