// Project Euler: Problem 79

/*
  I saw the Wikipedia page about topological sorting.
    https://en.wikipedia.org/wiki/Topological_sorting

  Note: This implementation finds only one topological sort not all.
*/

use std::collections::HashMap;
use std::fs::File;
use std::io::{BufReader, Lines};

euler::run_solver!(79);

fn solve() -> String {
    compute("./assets/p079_keylog.txt").to_string()
}

fn compute(fname: &str) -> i64 {
    use euler::math;

    let data_it = match euler::read_lines(fname) {
        Ok(it) => it,
        Err(error) => panic!("Problem reading the file {}: {:?}", fname, error),
    };
    let mut graph = match parse_data(data_it) {
        Ok(graph) => graph,
        Err(error) => panic!("Problem parsing the file {}: {:?}", fname, error),
    };

    let mut acc: Vec<i64> = Vec::new();
    let keys: Vec<i64> = graph.keys().copied().collect();
    for v in keys {
        acc = dfs(&mut graph, &mut acc, v);
    }
    //acc.into_iter().rev().fold(String::new(), |acc, x| )

    acc.reverse();

    math::undigits(&acc)
}

fn parse_data(it: Lines<BufReader<File>>) -> Result<HashMap<i64, Vec<i64>>, std::io::Error> {
    let mut ret: HashMap<i64, Vec<i64>> = HashMap::new();

    for line_result in it {
        let v = line_result?
            .chars()
            .map(|ch| ch.to_digit(10).unwrap() as i64)
            .collect::<Vec<i64>>();
        ret.entry(v[0])
            .or_insert(Vec::new())
            .extend(vec![v[1], v[2]]);
        ret.entry(v[1]).or_insert(Vec::new()).push(v[2]);
    }
    Ok(ret)
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
        assert_eq!(compute("./assets/p079_keylog.txt"), 73162890);
    }
}
