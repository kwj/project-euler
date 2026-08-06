package p0001

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(limit int) string {
	var f = func(x int) int {
		var tmp = (limit - 1) / x
		return (1 + tmp) * tmp / 2 * x
	}

	return strconv.FormatInt(int64(f(3)+f(5)-f(mylib.Lcm(3, 5))), 10)
}

func Solve() string {
	return compute(1_000)
}
