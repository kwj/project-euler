package p0018

import (
	"slices"
	"strconv"
	"strings"
)

var triangleData = `
75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
`

func makeTriangle(data string) [][]int {
	ss := strings.Split(data, "\n")
	triangle := make([][]int, len(ss))
	for idx, line := range ss {
		row := make([]int, 0)
		tokens := strings.Split(line, " ")
		if len(tokens) != idx+1 {
			panic("data error (not triangle)")
		}
		for _, x := range tokens {
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
	for _, crnt := range triangle[1:] {
		selected := selectLargerValue(work[0:len(work)-1], work[1:])
		for idx := range crnt {
			crnt[idx] += selected[idx]
		}
		work = crnt
	}

	return strconv.FormatInt(int64(work[0]), 10)
}

func Solve() string {
	return compute(triangleData)
}
