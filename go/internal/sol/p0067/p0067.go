package p0067

import (
	_ "embed"
	"slices"
	"strconv"
	"strings"
)

//go:embed 0067_triangle.txt
var fileContent string

func makeTriangle(data string) [][]int {
	ss := strings.Split(data, "\n")
	triangle := make([][]int, len(ss))
	for idx, line := range slices.All(ss) {
		row := make([]int, 0)
		tokens := strings.Split(line, " ")
		if len(tokens) != idx+1 {
			panic("data error (not triangle)")
		}
		for x := range slices.Values(tokens) {
			if n, err := strconv.Atoi(x); err == nil {
				row = append(row, n)
			} else {
				panic("data error (not numeric character)")
			}
		}
		triangle[idx] = row
	}

	return triangle
}

func selectLargerValue(a, b []int) []int {
	if len(a) != len(b) {
		panic("slice length mismatch")
	}
	result := make([]int, len(a))
	for i := range len(a) {
		result[i] = max(a[i], b[i])
	}

	return result
}

func compute(data string) string {
	triangle := makeTriangle(strings.Trim(data, "\n"))
	slices.Reverse(triangle)

	work := triangle[0]
	for crnt := range slices.Values(triangle[1:]) {
		selected := selectLargerValue(work[0:len(work)-1], work[1:])
		for idx := range len(crnt) {
			crnt[idx] += selected[idx]
		}
		work = crnt
	}

	return strconv.FormatInt(int64(work[0]), 10)
}

func Solve() string {
	return compute(fileContent)
}
