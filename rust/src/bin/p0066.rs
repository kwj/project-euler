// Project Euler: Problem 66

/*
  X^2 - N * Y^2 = 1
  -----------------

  This equation is called Pell's equation, but I didn't know that.
  So I wrote a solution by referring Wikipedia.

    https://en.wikipedia.org/wiki/Pell%27s_equation


            sqrt(N) + b0        1              1
  sqrt(N) = ------------ = a0 + --,  x1 = a1 + --, ...
                 c0             x1             x2

                  c0             c0(sqrt(N) - (b0 - a0c0))
    x1 = --------------------- = -------------------------
         sqrt(N) + (b0 - a0c0)       N - (b0 - a0c0)^2

         sqrt(N) + (a0c0 - b0)   sqrt(N) + b1         1
       = --------------------- = ------------- = a1 + --
           N - (a0c0 - b0)^2          c1              x2
           -----------------
                  c0
   -->
     b{n+1} = a{n}*c{n} - b{n}
     c{n+1} = (N - b{n+1}^2) / c{n}
     a{n+1} = floor((sqrt(N)+b{n+1}) / c{n+1}) = (floor(sqrt(N)) + b{n+1}) / c{n+1}

       a{0} = floor(sqrt(N)), b{0} = 0, c{0} = 1


  write A{0}, A{1}, A{2}, ...
           x{0}                          x{1}                      1         x{2}
    a{0} = ---- = A{0},  a{0} + 1/a{1} = ---- = A{1},  a{0} + ------------ = ---- = A{2},  ...
           y{0}                          y{1}                         1      y{2}
                                                              a{1} + ----
                                                                     a{2}
                                              [x{0} = a{0}, y{0} = 1]
   -->
        n=0: -> x{0} = a{0}, y{0} = 1
        n=1: -> x{1} = a{0}*a{1} + 1, y{1} = a{1}

        n=2: -> x{2}/y{2}
                     = (a{0}*a{1}*a{2} + a{0} + a{2}) / (a{1}a{2} + 1)

                       a{2}*(a{0}*a{1} + 1) + a{0}   a{2}*x{1} + a{0}
                     = --------------------------- = ----------------
                            a{2}*a{1} + 1            a{2}*y(1) + 1

                       a{2}*x{1} + x{0}
                     = ----------------
                       a{2)*y{1} + y{0}

                                    a{k}*x{k-1} + x{k-2}
     assume that A{k} = x{k}/y{k} = --------------------  [k>=2]
                                    a{k}*y{k-1} + y{k-2}

                                 ((a{k}*a{k+1} + 1)/a{k+1})*x{k-1} + x{k-2}
        A{k+1} = x{k+1}/y{k+1} = -----------------------------------------
                                 ((a{k}*a{k+1} + 1)/a{k+1})*y{k-1} + y{k-2}

                                 (a{k}*a{k+1} + 1)*x{k-1} + x{k-2}*a{k+1}
                               = -----------------------------------------
                                 (a{k}*a{k+1} + 1)*y{k-1} + y{k-2}*a{k+1}

                                 a{k+1}(a{k}*x{k-1} + x{k-2}) + x{k-1}
                               = -------------------------------------
                                 a{k+1}(a{k}*y{k-1} + y{k-2}) + y{k-1}

                                 a{k+1}*x{k} + x{k-1}
                               = --------------------
                                 a{k+1}*y{k} + y{k-1}
      -->
        x{k+1} = a{k+1} * x{k} + x{k-1}
        y{k+1} = a{k+1} * y{k} + y{k-1}

   -->
     [a{0}; a{1], a{2}, ...]
     assume that x{-1} = 1, x{0} = a{0}, y{-1} = 0, y{0} = 1

     [n>=1]
       x{n} = a{n} * x{n-1} + x{n-2}
       y{n} = a{n} * y{n-1} + y{n-2}
*/

use num_bigint::BigUint;

euler::run_solver!(66);

fn solve() -> String {
    compute(1_000).to_string()
}

fn compute(limit: i64) -> i64 {
    debug_assert!(limit > 1);

    let mut ans: (BigUint, i64) = (BigUint::from(0_u32), 0);
    let mut numerator: BigUint;
    for i in 1..=limit {
        let (cf_a0, mut cf_rep) = get_cont_fraction(i);
        if cf_rep.is_empty() {
            continue;
        }
        if cf_rep.len() % 2 == 1 {
            cf_rep.extend(cf_rep.clone());
        }
        let end_pos = cf_rep.len() - 1;
        numerator = get_numerator(cf_a0, &cf_rep[0..end_pos]);
        if ans.0 < numerator {
            ans = (numerator, i)
        }
    }
    ans.1
}

fn get_cont_fraction(n: i64) -> (u64, Vec<u64>) {
    use euler::math;

    let isqrt_n = math::isqrt(n);
    if n == isqrt_n * isqrt_n {
        return (isqrt_n as u64, vec![]);
    }

    let stop_condition = isqrt_n * 2;
    let mut b = 0_i64;
    let mut c = 1_i64;
    let mut a = (isqrt_n + b) / c;
    let mut rep: Vec<u64> = Vec::new();
    loop {
        b = a * c - b;
        c = (n - b * b) / c;
        a = (isqrt_n + b) / c;
        rep.push(a as u64);
        if a == stop_condition {
            return (isqrt_n as u64, rep);
        }
    }
}

fn get_numerator(a0: u64, rep_lst: &[u64]) -> BigUint {
    let mut x = BigUint::from(a0);
    let mut y = BigUint::from(1_u32);

    for a in rep_lst {
        (x, y) = (*a * &x + y, x);
    }

    x
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0066_7() {
        assert_eq!(compute(7), 5);
    }

    #[test]
    fn p0066_1000() {
        assert_eq!(compute(1_000), 661);
    }
}
