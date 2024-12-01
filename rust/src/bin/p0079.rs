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

fn compute(data: &str) -> i64 {
    use euler::math;

    let mut graph = parse_data(data);
    let mut acc: Vec<i64> = Vec::new();
    let keys: Vec<i64> = graph.keys().copied().collect();

    for v in keys {
        acc = dfs(&mut graph, &mut acc, v);
    }
    acc.reverse();

    math::undigits(&acc)
}

fn parse_data(data: &str) -> HashMap<i64, Vec<i64>> {
    let mut ret: HashMap<i64, Vec<i64>> = HashMap::new();

    for line in data.lines() {
        let v: Vec<_> = line
            .chars()
            .map(|ch| i64::from(ch.to_digit(10).unwrap()))
            .collect();
        ret.entry(v[0]).or_default().extend(vec![v[1], v[2]]);
        ret.entry(v[1]).or_default().push(v[2]);
    }

    ret
}

fn dfs(graph: &mut HashMap<i64, Vec<i64>>, perm: &mut [i64], v: i64) -> Vec<i64> {
    fn visit(temp: &[i64], visited: &[i64], node: i64, graph: &HashMap<i64, Vec<i64>>) -> Vec<i64> {
        if temp.contains(&node) {
            panic!("a cycle path is found");
        }
        if visited.contains(&node) {
            return visited.to_vec();
        }

        if let Some(lst) = graph.get(&node) {
            let mut acc = visited.to_vec();
            for v in lst {
                acc = visit(&[vec![node], temp.to_vec()].concat(), &acc, *v, graph);
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
