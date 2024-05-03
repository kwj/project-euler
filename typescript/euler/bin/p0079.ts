// project euler: problem 79

import { assetData } from "../lib/asset.ts";

const dfs = (
  graph: Map<string, Set<string>>,
  perm: string[],
  v: string,
): string[] => {
  const visit = (temp: string[], visited: string[], node: string): string[] => {
    if (temp.includes(node) === true) {
      throw new Error("cycle path is found");
    }
    if (visited.includes(node) === true) {
      return visited;
    }
    if (graph.has(node) === true) {
      let acc = visited;
      for (const v of graph.get(node)!) {
        acc = visit([node].concat(temp), acc, v);
      }
      return [node].concat(acc);
    } else {
      return [node];
    }
  };

  return visit([], perm, v);
};

const parseData = (data: string): string[][] => {
  const splitLines = (str: string): string[] => {
    const result = str.split(/\r?\n/);
    if (result.at(-1) === "") {
      return result.slice(0, -1);
    } else {
      return result;
    }
  };

  return splitLines(data)
    .map((x) => x.split(""))
    .map((y) => [[y[0], y[1]], [y[0], y[2]], [y[1], y[2]]]).flat();
};

export const compute = (data: string): string => {
  const graph = new Map<string, Set<string>>();
  for (const [k, v] of parseData(data)) {
    graph.set(k, (graph.get(k) || new Set<string>()).add(v));
  }

  let acc: string[] = [];
  for (const v of graph.keys()) {
    acc = dfs(graph, acc, v);
  }

  return acc.join("");
};

export const solve = (): string => {
  const data = new TextDecoder().decode(assetData("p079_keylog.txt"));
  return compute(data);
};
