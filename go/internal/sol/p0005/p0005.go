package p0005

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(limit int) string {
	if limit <= 0 {
		panic("The argument must be positive")
	}

	var ans = 1
	for i := 2; i <= limit; i++ {
		ans = mylib.Lcm(ans, i)
	}

	return strconv.FormatInt(int64(ans), 10)
}

func Solve() string {
	return compute(20)
}
