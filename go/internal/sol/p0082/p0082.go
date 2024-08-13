package p0082

import (
	_ "embed"
	"slices"
	"strconv"
	"strings"
)

//go:embed 0082_matrix.txt
var fileContent string

func parseData(data string) [][]int {
	result := make([][]int, 0)

	for line := range slices.Values(strings.Split(strings.Trim(data, "\n"), "\n")) {
		tmp := make([]int, 0)
		for s := range slices.Values(strings.Split(line, ",")) {
			if n, err := strconv.Atoi(s); err == nil {
				tmp = append(tmp, n)
			}
		}
		result = append(result, tmp)
	}

	return result
}

func transpose[T any](matrix [][]T) [][]T {
	nCol := len(matrix[0])
	nRow := len(matrix)
	result := make([][]T, nCol)
	for i := range nCol {
		result[i] = make([]T, nRow)
	}
	for i := range nCol {
		for j := range nRow {
			result[i][j] = matrix[j][i]
		}
	}

	return result
}

func compute(data string) string {
	matrix := transpose(parseData(data))

	work := matrix[0]
	for crnt := range slices.Values(matrix[1:]) {
		work[0] += crnt[0]
		for i := 1; i < len(crnt); i++ {
			work[i] = crnt[i] + min(work[i], work[i-1])
		}
		for i := len(crnt) - 2; i >= 0; i-- {
			work[i] = min(work[i], work[i+1]+crnt[i])
		}
	}
	slices.Sort(work)

	return strconv.FormatInt(int64(work[0]), 10)
}

func Solve() string {
	return compute(fileContent)
}
