// Project Euler: Problem 17

euler::run_solver!(17);

fn solve() -> String {
    compute(1_000).to_string()
}

fn compute(limit: i64) -> usize {
    use std::collections::HashMap;

    debug_assert!(limit > 0 && limit <= 1_000);

    let kv_pairs: Vec<(i64, usize)> = vec![
        (1, "one".len()),
        (2, "two".len()),
        (3, "three".len()),
        (4, "four".len()),
        (5, "five".len()),
        (6, "six".len()),
        (7, "seven".len()),
        (8, "eight".len()),
        (9, "nine".len()),
        (10, "ten".len()),
        (11, "eleven".len()),
        (12, "twelve".len()),
        (13, "thirteen".len()),
        (14, "fourteen".len()),
        (15, "fifteen".len()),
        (16, "sixteen".len()),
        (17, "seventeen".len()),
        (18, "eighteen".len()),
        (19, "nineteen".len()),
        (20, "twenty".len()),
        (30, "thirty".len()),
        (40, "forty".len()),
        (50, "fifty".len()),
        (60, "sixty".len()),
        (70, "seventy".len()),
        (80, "eighty".len()),
        (90, "ninety".len()),
        (0, 0),
    ];
    let words: HashMap<_, _> = kv_pairs.into_iter().collect();
    let mut acc: usize = 0;

    for n in 1..=limit {
        if n == 1_000 {
            // one thousand (3 + 8)
            acc += 11;
        } else if n < 20 {
            acc += words.get(&n).unwrap();
        } else if n < 100 {
            acc += words.get(&(n - (n % 10))).unwrap() + words.get(&(n % 10)).unwrap();
        } else if n % 100 == 0 {
            // xxx hundred(7)
            acc += words.get(&(n / 100)).unwrap() + 7;
        } else if n % 100 < 20 {
            // xxx handred(7) and(3) ...
            acc += words.get(&(n / 100)).unwrap() + 7 + 3 + words.get(&(n % 100)).unwrap();
        } else {
            // xxx handred(7) and(3) ...
            acc += words.get(&(n / 100)).unwrap()
                + 7
                + 3
                + words.get(&((n % 100) - (n % 10))).unwrap()
                + words.get(&(n % 10)).unwrap();
        }
    }

    acc
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0017_limit_5() {
        assert_eq!(compute(5), 19);
    }

    #[test]
    fn p0017_limit_1000() {
        assert_eq!(compute(1_000), 21124);
    }
}
