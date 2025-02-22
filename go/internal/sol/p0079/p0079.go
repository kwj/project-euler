package p0079

import (
	_ "embed"
	"maps"
	"slices"
	"strings"
)

//go:embed 0079_keylog.txt
var fileContent string

// topological sorting
func dfs(graph map[string][]string, perm []string, v string) []string {
	var visit func([]string, []string, string) []string
	visit = func(temp []string, visited []string, node string) []string {
		if slices.Contains(temp, node) {
			panic("cycle detection")
		}
		if slices.Contains(visited, node) {
			return visited
		}
		if _, ok := graph[node]; ok {
			acc := visited
			for v := range slices.Values(graph[node]) {
				acc = visit(slices.Concat([]string{node}, temp), acc, v)
			}
			return slices.Concat([]string{node}, acc)
		} else {
			return []string{node}
		}
	}

	return visit([]string{}, perm, v)
}

func parseData(data string) map[string][]string {
	m := make(map[string][]string)

	for line := range strings.SplitSeq(strings.Trim(data, "\n"), "\n") {
		x := strings.Split(line, "")
		for kv := range slices.Values([][]string{{x[0], x[1]}, {x[0], x[2]}, {x[1], x[2]}}) {
			if v, ok := m[kv[0]]; ok {
				m[kv[0]] = append(v, kv[1])
			} else {
				m[kv[0]] = []string{kv[1]}
			}
		}
	}

	// sort & dedup
	for k, xs := range maps.All(m) {
		slices.Sort(xs)
		i := 0
		for j := 1; j < len(xs); j++ {
			if xs[i] == xs[j] {
				continue
			}
			i++
			xs[i] = xs[j]
		}
		m[k] = xs[:i+1]
	}

	return m
}

func compute(data string) string {
	graph := parseData(data)

	var acc []string
	for vertex := range maps.Keys(graph) {
		acc = dfs(graph, acc, vertex)
	}

	var result string
	for s := range slices.Values(acc) {
		result += s
	}

	return result
}

func Solve() string {
	return compute(fileContent)
}
