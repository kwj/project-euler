// Project Euler: Problem 57

/*
  use recurrence relation:
    sqrt(2) = 1 + sqrt(2) - 1
            = 1 + 1 / ( 1 / (sqrt(2) - 1) )
            = 1 + 1 / ( (sqrt(2) + 1) / (2 - 1) )
            = 1 + 1 / (1 + sqrt(2))
    -->
    a{1} = 1 + 1/2
    a{n} = 1 + 1/(1 + a{n-1})    [n>1]

  assume that b{n}/c{n} = a{n}
    b{1}/c{1} = 1 + 1/2 = 3/2
    b{n}/c{n} = 1 + 1/(1 + b{n-1}/c{n-1})
              = 1 + 1/((c{n-1) + b{n-1})/c{n-1})
              = 1 + c{n-1}/(c{n-1) + b{n-1})
              = (c{n-1) + b{n-1} + c{n-1))/(c{n-1) + b{n-1})
              = (2 * c{n-1} + b{n-1}) / (c{n-1) + b{n-1})
*/

euler::run_solver!(57);

fn solve() -> String {
    compute(1_000).to_string()
}

fn compute(limit: i64) -> i64 {
    use num_bigint::BigUint;

    debug_assert!(limit > 0);

    let mut ans: i64 = 0;
    let (mut b, mut c) = (BigUint::from(1_u32), BigUint::from(1_u32));

    for _ in 0..limit {
        (b, c) = (&c + &c + &b, &c + &b);
        if BigUint::to_str_radix(&b, 10).len() > BigUint::to_str_radix(&c, 10).len() {
            ans += 1;
        }
    }

    ans
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0057_8() {
        assert_eq!(compute(8), 1);
    }

    #[test]
    fn p0057_1000() {
        assert_eq!(compute(1_000), 153);
    }
}
