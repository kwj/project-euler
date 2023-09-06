// Project Euler: Problem 18

const DATA: &str = "
    75
    95 64
    17 47 82
    18 35 87 10
    20 04 82 47 65
    19 01 23 75 03 34
    88 02 77 73 07 63 67
    99 65 04 28 06 16 70 92
    41 41 26 56 83 40 80 70 33
    41 48 72 33 47 32 37 16 94 29
    53 71 44 65 25 43 91 52 97 51 14
    70 11 33 28 77 73 17 78 39 68 17 57
    91 71 52 38 17 14 91 43 58 50 27 29 48
    63 66 04 68 89 53 67 30 73 16 69 87 40 31
    04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
";

euler::run_solver!(18);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    let triangle: Vec<Vec<i64>> = DATA
        .trim()
        .lines()
        .map(|s| {
            s.trim()
                .split_ascii_whitespace()
                .map(|s| s.parse::<i64>().unwrap())
                .collect()
        })
        .rev()
        .collect();

    let mut prev = vec![0_i64; triangle[0].len() + 1];
    for lst in triangle {
        let selected = select_leaf(&prev);
        prev = lst
            .into_iter()
            .zip(selected.into_iter())
            .map(|(x, y)| x + y)
            .collect();
    }
    prev[0]
}

fn select_leaf(lst: &[i64]) -> Vec<i64> {
    use std::cmp;

    let mut result: Vec<i64> = Vec::new();
    let mut prev = lst[0];

    for i in lst.iter() {
        result.push(cmp::max(prev, *i));
        prev = *i;
    }
    result.remove(0);
    result
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0018() {
        assert_eq!(compute(), 1074);
    }
}
