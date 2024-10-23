// Project Euler: Problem 61

use std::collections::{HashMap, HashSet, VecDeque};

euler::run_solver!(61);

fn solve() -> String {
    compute(8).to_string()
}

fn compute(max_nsides_polygon: usize) -> i64 {
    debug_assert!(max_nsides_polygon > 3);

    let cycles: Vec<Vec<i64>> = find_closed_paths(max_nsides_polygon)
        .into_iter()
        .map(|path| {
            // Change a closed path to a vector of cyclic numbers
            path.windows(2)
                .map(|pair| pair[0] * 100 + pair[1])
                .collect::<Vec<i64>>()
        })
        .filter(|lst| {
            // All numbers in a cycle are different from each other's
            lst.len() == HashSet::<i64>::from_iter(lst.clone()).len()
        })
        .collect();

    // There exists only one cycle
    if cycles.len() == 1 {
        return cycles[0].iter().sum();
    }

    unreachable!();
}

fn find_closed_paths(max_nsides_polygon: usize) -> Vec<Vec<i64>> {
    let mut closed_paths: Vec<Vec<i64>> = Vec::new();
    let polynum_tbl: HashMap<usize, HashMap<i64, Vec<i64>>> = make_polynum_tbl(max_nsides_polygon);
    let stop_condition: u32 = (1 << (max_nsides_polygon + 1)) - 8;

    let mut get_next_states = |(bits, path): (u32, Vec<i64>)| -> Vec<(u32, Vec<i64>)> {
        let mut states: Vec<(u32, Vec<i64>)> = Vec::new();

        // bits: (when max_nsides_polygon = 8)
        //   0b######000
        //     ||||||
        //     |||||+- triangle
        //     ||||+-- square
        //     |||+--- pentagonal
        //     ||+---- hexagonal
        //     |+----- heptagonal
        //     +------ octagonal
        if bits == stop_condition {
            if path[0] == *path.last().unwrap() {
                closed_paths.push(path);
            }
        } else {
            for i in 3_usize..max_nsides_polygon {
                let p_bit = 0b1_u32 << i;
                if bits & p_bit != 0 {
                    continue;
                }
                let next_tbl = polynum_tbl.get(&i).unwrap();
                if let Some(vs) = next_tbl.get(path.last().unwrap()) {
                    for &x in vs {
                        let mut next_path = path.clone();
                        next_path.push(x);
                        states.push((bits | p_bit, next_path));
                    }
                }
            }
        }

        states
    };

    // Search for all closed paths
    let mut q: VecDeque<(u32, Vec<i64>)> = VecDeque::new();
    for (&k, vs) in polynum_tbl.get(&max_nsides_polygon).unwrap() {
        for &v in vs {
            q.push_back((0b1 << max_nsides_polygon, vec![k, v]));
        }
    }
    while !q.is_empty() {
        for (bits, path) in get_next_states(q.pop_front().unwrap()).into_iter() {
            q.push_front((bits, path));
        }
    }

    closed_paths
}

fn make_polynum_tbl(max_nsides_polygon: usize) -> HashMap<usize, HashMap<i64, Vec<i64>>> {
    let mut tbl: HashMap<usize, HashMap<i64, Vec<i64>>> = HashMap::new();
    for p in 3..=max_nsides_polygon {
        let mut x: HashMap<i64, Vec<i64>> = HashMap::new();
        for n in (1_i64..)
            .step_by(p - 2)
            .scan(0, |state, x| {
                *state += x;
                Some(*state)
            })
            .skip_while(|&x| x < 1_000)
            .take_while(|&x| x < 10_000)
            .filter(|&x| x % 100 >= 10)
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
    fn p0061_4() {
        assert_eq!(compute(4), 8181);
    }

    #[test]
    fn p0061_5() {
        assert_eq!(compute(5), 19291);
    }

    #[test]
    fn p0061_8() {
        assert_eq!(compute(8), 28684);
    }
}
