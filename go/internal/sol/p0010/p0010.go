package p0010

import (
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(upper int) string {
	var result int
	for v := range slices.Values(mylib.Primes(upper)) {
		result += v
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(2_000_000)
}
