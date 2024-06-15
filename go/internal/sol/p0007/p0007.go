package p0007

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(nth uint) string {
	if nth == 0 {
		panic("The argument must be positive")
	}

	pGen := mylib.PrimeGenerator{}

	// Drop 'nth - 1' prime numbers
	for range nth - 1 {
		_ = pGen.Next()
	}

	return strconv.FormatUint(uint64(pGen.Next()), 10)
}

func Solve() string {
	return compute(10_001)
}
