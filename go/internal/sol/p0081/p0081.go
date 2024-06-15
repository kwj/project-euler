package p0081

import (
	_ "embed"
	"strconv"
	"strings"
)

//go:embed 0081_matrix.txt
var fileContent string

func parseData(data string) [][]int {
	result := make([][]int, 0)

	for _, line := range strings.Split(strings.Trim(data, "\n"), "\n") {
		tmp := make([]int, 0)
		for _, s := range strings.Split(line, ",") {
			if n, err := strconv.Atoi(s); err == nil {
				tmp = append(tmp, n)
			}
		}
		result = append(result, tmp)
	}

	return result
}

func compute(data string) string {
	const maxInt = int(^uint(0) >> 1) // sentinel

	accumulate := func(xs []int) []int {
		result := make([]int, 0)
		acc := 0
		for _, n := range xs {
			acc += n
			result = append(result, acc)
		}

		return result
	}

	matrix := parseData(data)
	prev := append([]int{maxInt}, accumulate(matrix[0])...)

	for _, work := range matrix[1:] {
		work = append([]int{maxInt}, work...)
		for i := 1; i < len(work); i++ {
			work[i] += min(work[i-1], prev[i])
		}
		prev = work
	}

	return strconv.FormatInt(int64(prev[len(prev)-1]), 10)
}

func Solve() string {
	return compute(fileContent)
}
