package p0006

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func sumOfSquares(n int) int {
	return n * (n + 1) * (2*n + 1) / 6
}

func squareOfSum(n int) int {
	return mylib.Pow((n * (n + 1) / 2), 2)
}

func compute(limit int) string {
	if limit <= 0 {
		panic("The argument must be positive")
	}

	// The square of sum is equal or larger than the sum of squares.
	return strconv.FormatUint(
		uint64(squareOfSum(limit) - sumOfSquares(limit)),
		10,
	)
}

func Solve() string {
	return compute(100)
}
