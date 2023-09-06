// Project Euler: Problem 83

/*
  https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
*/

use std::fs::File;
use std::io::{BufReader, Lines};
use std::{cmp::Ordering, collections::BinaryHeap};

euler::run_solver!(83);

fn solve() -> String {
    compute("./assets/p083_matrix.txt").to_string()
}

fn compute(fname: &str) -> i64 {
    let data_it = match euler::read_lines(fname) {
        Ok(it) => it,
        Err(error) => panic!("Problem reading the file {}: {:?}", fname, error),
    };
    let matrix = match parse_data(data_it) {
        Ok(matrix) => matrix,
        Err(error) => panic!("Problem parsing the file {}: {:?}", fname, error),
    };
    let nbr_tbl = make_neighbor_tbl(matrix.len(), matrix[0].len());
    let mut dist_tbl = make_distance_tbl(matrix.len(), matrix[0].len());
    dist_tbl[0][0] = matrix[0][0];

    let mut pq: BinaryHeap<Node> = BinaryHeap::new();
    pq.push(Node {
        priority: dist_tbl[0][0],
        pos: (0, 0),
    });

    while !pq.is_empty() {
        let Node {
            priority: d,
            pos: (i, j),
        } = pq.pop().unwrap();
        for (x, y) in nbr_tbl[i][j].iter() {
            let new_d = d + matrix[*x][*y];
            if new_d < dist_tbl[*x][*y] {
                dist_tbl[*x][*y] = new_d;
                pq.push(Node {
                    priority: new_d,
                    pos: (*x, *y),
                });
            }
        }
    }
    dist_tbl[dist_tbl.len() - 1][dist_tbl[0].len() - 1]
}

fn parse_data(it: Lines<BufReader<File>>) -> Result<Vec<Vec<i64>>, std::io::Error> {
    let mut ret: Vec<Vec<i64>> = Vec::new();

    for line_result in it {
        let row = line_result?
            .split(',')
            .map(|s| s.parse::<i64>().unwrap())
            .collect();
        ret.push(row);
    }
    Ok(ret)
}

fn make_neighbor_tbl(n_rows: usize, n_cols: usize) -> Vec<Vec<Vec<(usize, usize)>>> {
    let mut tbl = vec![vec![Vec::new() as Vec<(usize, usize)>; n_cols]; n_rows];

    for (r_idx, rows) in tbl.iter_mut().enumerate() {
        for (c_idx, node) in rows.iter_mut().enumerate() {
            let x = r_idx as i64;
            let y = c_idx as i64;
            *node = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
                .into_iter()
                .filter(|&tpl| {
                    tpl.0 >= 0 && tpl.0 < n_rows as i64 && tpl.1 >= 0 && tpl.1 < n_cols as i64
                })
                .map(|tpl| (tpl.0 as usize, tpl.1 as usize))
                .collect();
        }
    }
    tbl
}

fn make_distance_tbl(n_rows: usize, n_cols: usize) -> Vec<Vec<i64>> {
    vec![vec![i64::MAX; n_cols]; n_rows]
}

// minimum binary heap (priority key: distance from (0, 0), value: node)
#[derive(Debug, Clone, PartialEq, Eq)]
struct Node {
    priority: i64,
    pos: (usize, usize),
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        other.priority.cmp(&self.priority)
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        other.priority.partial_cmp(&self.priority)
    }
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0083() {
        assert_eq!(compute("./assets/p083_matrix.txt"), 425185);
    }
}
