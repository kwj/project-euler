// Project Euler: Problem 79

/*
  I saw the Wikipedia page about topological sorting.
    https://en.wikipedia.org/wiki/Topological_sorting

  Note: This implementation finds only one topological sort not all.
*/

use std::collections::HashMap;

euler::run_solver!(79);

static FILE_DATA: &str = include_str!("../../assets/0079_keylog.txt");

fn solve() -> String {
    compute(FILE_DATA).to_string()
}

fn compute(data: &str) -> u64 {
    use euler::math;

    let mut graph = parse_data(data);
    let mut acc: Vec<u64> = Vec::new();
    let keys: Vec<u64> = graph.keys().copied().collect();

    for v in keys {
        acc = dfs(&mut graph, &mut acc, v);
    }
    acc.reverse();

    math::undigits(&acc)
}

fn parse_data(data: &str) -> HashMap<u64, Vec<u64>> {
    let mut ret: HashMap<u64, Vec<u64>> = HashMap::new();

    for line in data.lines() {
        let v: Vec<_> = line
            .chars()
            .map(|ch| u64::from(ch.to_digit(10).unwrap()))
            .collect();
        ret.entry(v[0]).or_default().extend(vec![v[1], v[2]]);
        ret.entry(v[1]).or_default().push(v[2]);
    }

    ret
}

fn dfs(graph: &mut HashMap<u64, Vec<u64>>, perm: &mut [u64], v: u64) -> Vec<u64> {
    fn visit(temp: &[u64], visited: &[u64], node: u64, graph: &HashMap<u64, Vec<u64>>) -> Vec<u64> {
        assert!(!temp.contains(&node), "a cycle path is found");

        if visited.contains(&node) {
            return visited.to_vec();
        }

        if let Some(lst) = graph.get(&node) {
            let mut acc = visited.to_vec();
            for &v in lst {
                acc = visit(&[vec![node], temp.to_vec()].concat(), &acc, v, graph);
            }
            [vec![node], acc].concat()
        } else {
            vec![node]
        }
    }

    visit(&Vec::new(), perm, v, graph)
}

#[cfg(test)]
mod tests {
    use super::compute;

    #[test]
    fn p0079() {
        assert_eq!(compute(super::FILE_DATA), 73162890);
    }
}
