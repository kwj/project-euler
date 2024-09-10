// Project Euler: Problem 61

use std::collections::{HashMap, HashSet};

euler::run_solver!(61);

fn solve() -> String {
    compute().to_string()
}

fn compute() -> i64 {
    use itertools::Itertools;

    let polynum_tbl = make_polynum_tbl();

    // The reason for using rev() is to start with a route that has fewer options.
    for route in (3..=7).rev().permutations(5) {
        // according to the problem statement, only one cycle exists
        if let Some(x) = find_cycle(&route, &polynum_tbl) {
            // sum(100*x{1} + x{2}, 100*x{2} + x{3}, ..., 100*x{n} + x{1})
            //   = sum(x{1}, x{2}, ..., x{n}) * 101
            return x.iter().sum::<i64>() * 101;
        }
    }

    unreachable!();
}

fn find_cycle(
    route: &[usize],
    polynum_tbl: &HashMap<usize, HashMap<i64, Vec<i64>>>,
) -> Option<Vec<i64>> {
    fn is_distinct_numbers(nums: &[i64]) -> bool {
        let mut tmp: HashSet<i64> = HashSet::new();
        for i in 0..(nums.len() - 1) {
            tmp.insert(nums[i] * nums[i + 1]);
        }
        tmp.len() == (nums.len() - 1)
    }

    fn dfs(
        route: &[usize],
        tracks: Vec<i64>,
        tbl: &HashMap<usize, HashMap<i64, Vec<i64>>>,
    ) -> Option<Vec<i64>> {
        if route.is_empty() {
            if tracks[0] == *tracks.last().unwrap() && is_distinct_numbers(&tracks) {
                return Some(tracks[1..].to_vec());
            } else {
                return None;
            }
        }
        let next_map = tbl.get(&route[0]).unwrap();
        if !next_map.contains_key(tracks.last().unwrap()) {
            return None;
        }
        for next_num in next_map.get(tracks.last().unwrap()).unwrap() {
            let mut next_tracks = tracks.clone();
            next_tracks.push(*next_num);
            if let Some(res) = dfs(&route[1..], next_tracks, tbl) {
                return Some(res);
            }
        }
        None
    }

    for (k, v) in polynum_tbl.get(&8).unwrap().iter() {
        for next_num in v.iter() {
            if let Some(res) = dfs(route, vec![*k, *next_num], polynum_tbl) {
                return Some(res);
            }
        }
    }

    None
}

fn make_polynum_tbl() -> HashMap<usize, HashMap<i64, Vec<i64>>> {
    let mut tbl: HashMap<usize, HashMap<i64, Vec<i64>>> = HashMap::new();
    for p in 3..=8 {
        let mut x: HashMap<i64, Vec<i64>> = HashMap::new();
        for n in (1_i64..)
            .step_by(p - 2)
            .scan(0, |state, x| {
                *state += x;
                Some(*state)
            })
            .skip_while(|&x| x < 1_000)
            .take_while(|&x| x < 10_000)
            .filter(|&x| x % 100 > 10)
        {
            x.entry(n / 100).or_default().push(n % 100);
        }
        tbl.insert(p, x);
    }

    tbl
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0061() {
        assert_eq!(compute(), 28684);
    }
}
