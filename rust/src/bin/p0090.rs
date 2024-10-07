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
        .filter(|two_dice| check_square(two_dice))
        .count()
}

fn check_square(dice: &[Vec<i32>]) -> bool {
    debug_assert!(dice.len() == 2);

    // Squares list which is replaced 9's with 6
    const SQUARES: [[i32; 2]; 8] = [
        [0, 1],
        [0, 4],
        [0, 6],
        [1, 6],
        [2, 5],
        [3, 6],
        [4, 6],
        // [6, 4],  8^2 = 64, 7^2 = 49 -> 46.  So, this term is omitted.
        [8, 1],
    ];

    SQUARES.iter().all(|die| {
        dice[0].contains(&die[0]) && dice[1].contains(&die[1])
            || dice[0].contains(&die[1]) && dice[1].contains(&die[0])
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
