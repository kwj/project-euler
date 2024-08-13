package p0016

import (
	"math/big"
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(b, e int) string {
	if e < 0 {
		panic("exponent must not be negative")
	}

	var result int
	for v := range slices.Values(mylib.DigitsBig(mylib.PowBig(big.NewInt(int64(b)), e))) {
		result += v
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(2, 1_000)
}
