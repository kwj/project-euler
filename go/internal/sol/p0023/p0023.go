package p0023

import (
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

func abndntTbl(limit int) []bool {
	tbl := mylib.SigmaTbl(1, limit)
	for i := 1; i < len(tbl); i++ {
		tbl[i] -= i
	}
	tbl[0] = 0

	result := make([]bool, len(tbl))
	for x := range len(tbl) {
		if x < tbl[x] {
			result[x] = true
		}
	}

	return result
}

func compute(limit int) string {
	tbl := abndntTbl(limit)
	lst := make([]int, 0)
	var result int

loop:
	for i := 1; i < len(tbl); i++ {
		if i&1 == 0 && tbl[i/2] {
			lst = append(lst, i/2)
		}
		for x := range slices.Values(lst) {
			if tbl[i-x] {
				continue loop
			}
		}
		result += i
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(28_123)
}
