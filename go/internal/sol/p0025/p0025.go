package p0025

import (
	"math/big"
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(ndigits int) string {
	if ndigits < 0 {
		panic("exponent must not be negative")
	}

	thr := mylib.PowBig(big.NewInt(10), ndigits-1)
	nth := 2
	fib1, fib2, tmp := big.NewInt(1), big.NewInt(1), big.NewInt(0)
	for ; fib2.Cmp(thr) < 0; nth++ {
		tmp.Set(fib2)
		fib2.Add(fib2, fib1)
		fib1.Set(tmp)
	}

	return strconv.FormatInt(int64(nth), 10)
}

func Solve() string {
	return compute(1_000)
}
