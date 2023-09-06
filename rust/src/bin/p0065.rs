// Project Euler: Problem 65

/*
  e = [2; 1, 2, 1, 1, 4, 1, 1, 6, ..., 1, 1, 2k, ...]
      [a{0}; a{1}, a{2}, ...]

    i  a{i-1}  n(numerator)  d(denominator)
   ----------------------------------------
    1   2         2             1
    2   1         3             1
    3   2         8             3
    4   1        11             4
    5   1        19             7
    6   4        87            32
    7   1       106            39
    8   1       193            71
    9   6      1264           465
   10   1      1457           536
             ...
    i c(i)     n(i)          d(i)

    when i > 2:
      n(i) = n(i-1)*c(i) + n(i-2), n(2) = 3, n(1) = 2
      d(i) = d(i-1)*c(i) + d(i-2), d(2) = 1, d(1) = 1

      c(i) = | 1    (i mod 3 <> 0)
             | 2i/3 (i mod 3 = 0)

  ----
  def napier_cf_gen():
      yield 2
      for i in count(2):
          yield 1 if i % 3 != 0 else (2 * i) // 3
*/

euler::run_solver!(65);

fn solve() -> String {
    compute(100).to_string()
}

fn compute(nth: u32) -> i64 {
    use num_bigint::BigUint;

    fn c(i: u32) -> BigUint {
        if i % 3 != 0 {
            BigUint::from(1_u32)
        } else {
            BigUint::from(i) * 2_u32 / 3_u32
        }
    }

    let (mut n1, mut n2) = (BigUint::from(3_u32), BigUint::from(2_u32));
    for i in 3..=nth {
        (n1, n2) = (c(i) * &n1 + n2, n1);
    }
    n1.to_radix_le(10).into_iter().map(|x| x as i64).sum()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0065_10() {
        assert_eq!(compute(10), 17);
    }

    #[test]
    fn p0065_100() {
        assert_eq!(compute(100), 272);
    }
}
