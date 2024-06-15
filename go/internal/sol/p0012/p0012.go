package p0012

/*
triangle number's formula is n(n + 1)/2 and 'n' and 'n + 1' are coprime.
Therefore, ...
  - 'n/2' and 'n+1' are coprime (when 'n' is even)
  - 'n' and '(n+1)/2' are coprime (when 'n' is odd)

assume that f(n) returns number of divisors of 'n'.
f(a*b) = f(a) * f(b) when 'a' and 'b' are coprime.
*/

import (
	"strconv"

	"pe-solver/internal/mylib"
)

var memo = make(map[int]int)

func numOfDivisors(n int) int {
	if x, ok := memo[n]; ok {
		return x
	}

	var result = 1
	for _, v := range mylib.Frequencies(mylib.PrimeFactorization(n)) {
		result *= v + 1
	}
	memo[n] = result

	return result
}

func compute(thr int) string {
	var n = 1
	for {
		// n: odd number
		if numOfDivisors(n)*numOfDivisors((n+1)/2) > thr {
			break
		}
		n++

		// n: even number
		if numOfDivisors(n/2)*numOfDivisors(n+1) > thr {
			break
		}
		n++
	}

	return strconv.FormatInt(int64(n*(n+1)/2), 10)
}

func Solve() string {
	return compute(500)
}
