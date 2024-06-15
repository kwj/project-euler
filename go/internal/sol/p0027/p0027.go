package p0027

/*
n^2 + an + b,  where abs(a) < 1000 and abs(b) < 1000

when 'n' = 0:
  '0 + 0 + b' = 'b' must be a prime number. so, 2 < 'b' < 1000.
      'b' must not be 2
      if 'n' is an even number, the value of the expression becomes even
when 'n' = 1:
  '1 + a + b' must be a prime number.
  write this prime number is 'x', then 'a' = 'x' - 'b' - 1.
      abs('x' - b - 1) < 1000 and 2 < 'b' < 1000 ===> 0 < 'x' < 2000
when 'n' is a odd number:
  'n^2 + b' is a even number. so 'a' must be a odd number.
*/

import (
	"strconv"

	"pe-solver/internal/mylib"
)

func countConsecutive(a, b int) int {
	var cnt int
	for mylib.IsPrime(cnt*cnt + a*cnt + b) {
		cnt++
	}

	return cnt
}

type pair struct {
	a int
	b int
}

func compute() string {
	primes := mylib.Primes(2_000)
	var maxLen int
	var maxTpl pair

	for _, b := range primes[1:] {
		if b >= 1000 {
			break
		}
		for _, x := range primes {
			if a := x - b - 1; mylib.Abs(a) < 1000 {
				if tmp := countConsecutive(a, b); tmp > maxLen {
					maxLen = tmp
					maxTpl.a, maxTpl.b = a, b
				}
			}
		}
	}

	return strconv.FormatInt(int64(maxTpl.a*maxTpl.b), 10)
}

func Solve() string {
	return compute()
}
