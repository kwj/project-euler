package p0072

/*
Please see the following page.
https://mathproblems123.wordpress.com/2018/05/10/sum-of-the-euler-totient-function/
*/

import (
	"strconv"

	"pe-solver/internal/mylib"
)

var memo = make(map[int]int)

func sumPhi(n int) int {
	if v, ok := memo[n]; ok {
		return v
	}

	v := n * (n + 1) / 2
	for m := 2; m <= mylib.Isqrt(n); m++ {
		v -= sumPhi(n / m)
	}
	for d := 1; d <= n/(mylib.Isqrt(n)+1); d++ {
		v -= ((n / d) - (n / (d + 1))) * sumPhi(d)
	}
	memo[n] = v

	return v
}

func compute(limit int) string {
	result := sumPhi(limit) - sumPhi(1)

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(1_000_000)
}
