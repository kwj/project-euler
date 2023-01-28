
// project euler: problem 79

import { assetData } from "../lib/asset.ts";

function dfs(graph: Map<string, Set<string>>, perm: string[], v: string): string[] {
  function visit(temp: string[], visited: string[], node: string): string[] {
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
  }

  return visit([], perm, v);
}

function parseData(data: string): string[][] {
  function splitLines(str: string): string[] {
    const result = str.split(/\r?\n/);
    if (result.at(-1) === "") {
      return result.slice(0, -1);
    } else {
      return result;
    }
  }

  return splitLines(data).map((x) => x.split("")).map((y) => [[y[0], y[1]], [y[0], y[2]], [y[1], y[2]]]).flat();
}

export function compute(data: string): string {
  const graph = new Map<string, Set<string>>();
  for (const [k, v] of parseData(data)) {
    graph.set(k, (graph.get(k) || new Set<string>()).add(v));
  }

  let acc: string[] = [];
  for (const v of graph.keys()) {
    acc = dfs(graph, acc, v);
  }

  return acc.join("");
}

export function solve(): void {
  try {
    const data = new TextDecoder().decode(assetData("p079_keylog.txt"));

    const t0 = performance.now();
    const result = compute(data);
    const t1 = performance.now();
    const duration_ms = (t1 - t0).toFixed(4);

    console.log(`Answer: ${result}`);
    console.log(`Elapsed time: ${duration_ms} msec.`);
  } catch (err) {
    console.error(err.message);
  }

  return;
}
