// Project Euler: Problem 90

euler::run_solver!(90);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> usize {
    use itertools::Itertools;

    vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 6]
        .into_iter()
        .combinations(6)
        .combinations_with_replacement(2)
        .filter(|dice| check_square(dice))
        .count()
}

fn check_square(two_dice: &[Vec<i32>]) -> bool {
    assert!(two_dice.len() == 2);

    // Squared list with 9 replaced by 6
    const SQUARES: [[[i32; 2]; 2]; 8] = [
        [[1, 0], [0, 1]],
        [[4, 0], [0, 4]],
        [[6, 0], [0, 6]],
        [[6, 1], [1, 6]],
        [[5, 2], [2, 5]],
        [[6, 3], [3, 6]],
        [[6, 4], [4, 6]],
        // [[4, 6], [6, 4]],  8^2 = 64, 7^2 = 49 -> 46.  So this check can be omitted.
        [[1, 8], [8, 1]],
    ];

    SQUARES.iter().all(|arr| {
        arr.iter()
            .any(|sq| two_dice[0].contains(&sq[0]) && two_dice[1].contains(&sq[1]))
    })
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0090() {
        assert_eq!(compute(), 1217);
    }
}
