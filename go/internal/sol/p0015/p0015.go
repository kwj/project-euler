package p0015

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(r, c int) string {
	return strconv.FormatInt(int64(mylib.Binomial(r+c, c)), 10)
}

func Solve() string {
	return compute(20, 20)
}
