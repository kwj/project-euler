package p0056

import (
	"math/big"
	"strconv"

	"pe-solver/internal/mylib"
)

func compute() string {
	var result int
	for a := 100; a >= 1; a-- {
		if a%10 == 0 {
			continue
		}

		big_a := big.NewInt(int64(a))
		for b := 100; b >= 1; b-- {
			prod := mylib.PowBig(big_a, b)
			if len(prod.String())*9 < result {
				break
			}
			var sum int
			for _, v := range mylib.DigitsBig(prod) {
				sum += v
			}
			result = max(result, sum)
		}
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute()
}
