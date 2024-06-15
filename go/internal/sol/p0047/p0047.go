package p0047

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func compute(length int) string {
	if length < 2 {
		panic("invalid argument")
	}

	var cnt int
	for x := 1; ; x++ {
		switch {
		case len(mylib.Frequencies(mylib.PrimeFactorization(x))) != length:
			cnt = 0
		case cnt == length-1:
			return strconv.FormatInt(int64(x-length+1), 10)
		default:
			cnt++
		}
	}
}

func Solve() string {
	return compute(4)
}
