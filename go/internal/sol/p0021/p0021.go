package p0021

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(limit int) string {
	tbl := mylib.SigmaTbl(1, limit)
	for i := 1; i < len(tbl); i++ {
		tbl[i] -= i
	}

	var result int
	for x := 2; x < limit; x++ {
		if x > tbl[x] && tbl[tbl[x]] == x {
			result += x + tbl[x]
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(10_000)
}
