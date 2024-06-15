package p0020

import (
	"math/big"
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(n int) string {
	if n < 0 {
		panic("argument must not be negative")
	}

	x := big.NewInt(1)
	for n > 1 {
		x.Mul(x, big.NewInt(int64(n)))
		n--
	}

	var result int
	for _, v := range mylib.DigitsBig(x) {
		result += v
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(100)
}
