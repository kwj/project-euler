// Project Euler: Problem 2

/*
 f₁ = 1, f₂ = 2, f₃ = 3, f₄ = 5, f₅ = 8, f₆ = 13, f₇ = 21, f₈ = 34, f₉ = 55, ...

 Even numbers are present for every three items.
 Assume that k ≥ 7:
   f(k) = f(k-1) + f(k-2)
        = 2f(k-2) + f(k-3)
        = 2(f(k-3) + f(k-4)) + f(k-3)
        = 3f(k-3) + 2f(k-4)
        = 3f(k-3) + 2f(k-5) + 2f(k-6)
        = 4f(k-3) - f(k-3) + 2f(k-5) + 2f(k-6)
        = 4f(k-3) - (f(k-4) + f(k-5)) + 2f(k-5) + 2f(k-6)
        = 4f(k-3) - f(k-4) + f(k-5) + 2f(k-6)
        = 4f(k-3) - f(k-4) + (f(k-5) + f(k-6)) + f(k-6)
        = 4f(k-3) - f(k-4) + f(k-4) + f(k-6)
        = 4f(k-3) + f(k-6)
*/

euler::run_solver!(2);

fn solve() -> String {
    compute(4_000_000).to_string()
}

fn compute(limit: i64) -> i64 {
    let mut tpl: (i64, i64) = (2, 8);
    std::iter::from_fn(|| {
        let (a, b) = tpl;
        tpl = (b, 4 * b + a);
        Some(a)
    })
    .take_while(|&x| x <= limit)
    .sum()
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0002_under_100() {
        assert_eq!(compute(100), 44);
    }

    #[test]
    fn p0002_under_four_million() {
        assert_eq!(compute(4_000_000), 4613732);
    }
}
