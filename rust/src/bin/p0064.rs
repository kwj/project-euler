// Project Euler: Problem 64

/*
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
     a{n} = floor( (sqrt(N)+b{n}) / c{n} )
     b{n+1} = a{n}*c{n} - b{n}
     c{n+1} = (N - b{n+1}^2) / c{n}

     b{0} = 0, c{0} = 1, a{0} = sqrt(N)
*/

euler::run_solver!(64);

fn solve() -> String {
    compute(10_000).to_string()
}

fn compute(limit: i64) -> usize {
    (1..=limit)
        .filter(|&n| get_cont_fraction(n).1.len() % 2 == 1)
        .count()
}

fn get_cont_fraction(n: i64) -> (i64, Vec<i64>) {
    use euler::math;

    let isqrt_n = math::isqrt(n);
    if n == isqrt_n * isqrt_n {
        return (isqrt_n, vec![]);
    }

    let stop_condition = isqrt_n * 2;
    let mut b = 0_i64;
    let mut c = 1_i64;
    let mut a = (isqrt_n + b) / c;
    let mut rep: Vec<i64> = Vec::new();
    loop {
        b = a * c - b;
        c = (n - b * b) / c;
        a = (isqrt_n + b) / c;
        rep.push(a);
        if a == stop_condition {
            return (isqrt_n, rep);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0064_13() {
        assert_eq!(compute(13), 4);
    }

    #[test]
    fn p0064_10000() {
        assert_eq!(compute(10_000), 1322);
    }
}
