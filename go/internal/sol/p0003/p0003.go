package p0003

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(n int) string {
	lst := mylib.PrimeFactorization(n)

	return strconv.FormatInt(int64(lst[len(lst)-1]), 10)
}

func Solve() string {
	// 2**39 < 600_851_475_143 < 2**40
	// so, this solution can't be used on 32-bit environment
	return compute(600_851_475_143)
}
