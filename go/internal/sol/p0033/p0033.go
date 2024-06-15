package p0033

/*
case #1: (10a + c) / (10c + b) = a / b
case #2: (10a + c) / (10b + c) = a / b
case #3: (10c + a) / (10c + b) = a / b
case #4: (10c + a) / (10b + c) = a / b
// prerequisite: a < b

case #1:
  10ab + bc = 10ac + ab
  -> 9ab = 10ac - bc
  -> 9ab - 9ac = ac - bc
  -> 9a(b - c) = c(a - b)
  -> 9a(c - b) = c(b - a)  [a < b => a < b < c]

case #2:
  10ab + bc = 10ab + ac
  -> bc = ac
  -> b = a   ==> NG (contradiction)

case #3:
  10bc + ab = 10ac + ab
  -> 10bc = 10ac
  -> b = a   ==> NG (contradiction)

case #4:
  10bc + ab = 10ab + ac
  -> 10bc - ac = 9ab
  -> bc - ac = 9ab - 9bc
  -> c(b - a) = 9b(a - c)  [a < b => c < a < b]
  -> a - c = c/9 - ac/9b => 1   ==> NG (bacause c/9 < 1)

We only need to search for the case #1.
*/

import (
	"strconv"

	"pe-solver/internal/mylib"
)

type pair struct {
	a int
	b int
}

func makeCands() []pair {
	check := func(a, b, c int) bool {
		return 9*a*(c-b) == c*(b-a)
	}

	result := make([]pair, 0)
	ch := mylib.Combinations([]int{1, 2, 3, 4, 5, 6, 7, 8, 9}, 3)
	for lst := range ch {
		if check(lst[0], lst[1], lst[2]) {
			result = append(result, pair{lst[0], lst[1]})
		}
	}

	return result
}

func compute() string {
	// numerator, denominator
	a, b := 1, 1

	for _, tpl := range makeCands() {
		a *= tpl.a
		b *= tpl.b
	}

	return strconv.FormatInt(int64(b/mylib.Gcd(a, b)), 10)
}

func Solve() string {
	return compute()
}
