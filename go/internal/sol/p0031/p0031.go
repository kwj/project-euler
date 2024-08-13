package p0031

import (
	"slices"
	"strconv"
)

func compute(numbers []int, target int) string {
	tbl := make([]int, target+1)
	tbl[0] = 1

	for c := range slices.Values(numbers) {
		for i := c; i <= target; i++ {
			tbl[i] += tbl[i-c]
		}
	}

	return strconv.FormatInt(int64(tbl[len(tbl)-1]), 10)
}

func Solve() string {
	return compute([]int{1, 2, 5, 10, 20, 50, 100, 200}, 200)
}
