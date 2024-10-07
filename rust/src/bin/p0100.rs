// Project Euler: Problem 100

/*
  https://en.wikipedia.org/wiki/Pell%27s_equation#The_negative_Pell_equation

  number of blue disks: a
  number of total disks: b

  P(BB) = a/b * (a-1)/(b-1) = 1/2
  --> 2a(a-1) = b(b-1)
      2a^2 - 2a = b^2 - b
      4 * (2a^2 - 2a) = 4 * (b^2 - b)
      2 * (2a - 1)^2 - 2 = (2b - 1)^2 - 1
      (2b-1)^2 - 2(2a-1)^2 = -1

      x=2b-1, y=2a-1  [a = (y+1)/2]
      --> x^2 - 2y^2 = -1  [x,y are odd numbers] --- a negative pell's equation
          (x,y) = (1,1) -> 1 - 2*1 = -1

          (1+1*sqrt(2))^3 = (3+2*sqrt(2)) * (1+1*sqrt(2))
          --> x{n} = 3 * x{n-1} + 2 * 2 * y{n-1}
              y{n} = 2 * x{n-1} + 3 * y{n-1}

              if both x{1}, y{1} are odd, all x{n}, y{n} are odd too.

*/

euler::run_solver!(100);

fn solve() -> String {
    compute(1_000_000_000_000).to_string()
}

fn compute(thr: i64) -> i64 {
    let limit = 2 * thr - 1;
    let mut x: i64 = 1;
    let mut y: i64 = 1;

    while x <= limit {
        (x, y) = (3 * x + 4 * y, 2 * x + 3 * y);
    }

    (y + 1) / 2
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0100() {
        assert_eq!(compute(1_000_000_000_000), 756872327473);
    }
}
