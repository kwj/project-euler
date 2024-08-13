package p0092

/*
  This problem can be solved by combination and permutation because
  the next chain is determined by combination of numeric digit.

  Once a combination of digit numbers is determined, the total number of
  numbers represented by the combination, i.e., the number of permutations
  of multisets, can be obtained.

    n! / (k{1}! * k{2}! * ... * k{n}!)   [where n = k{1} + k{2} + ... + k{n}]

  For exapmle, we assume that a 4-digit combination with repetition is {1, 2, 2, 3}.

  They can be considered as one group since next chain of all of these
  numbers is 18 (1^2 + 2^2 + 2^2 + 3^2). Note that the final number in
  this chain is 89.

  There are 12 numbers presented by the combination as following.

    1223, 1232, 1322, 2123, 2132, 2213,
    2231, 2312, 2321, 3122, 3212, 3221

  The value of 12 can be obtained from above permutations with repetitions formula:

    {num of digits}! / ({num of '1'}! * {num of '2}! * {num of '3'}!)
      = 4! / (1! * 2! * 1!)
      = 24 / 2
      = 12

  On the other hand, we assume that an another combination with repetition is {1, 2, 3, 3}.
  There are 12 numbers from the combination in the same way.

    1233, 1323, 1332, 2133, 2313, 2331,
    3123, 3132, 3213, 3231, 3312, 3321

  However, the chain from this combination {1, 2, 3, 3} arrives at 1.
  We can therefore ignore the combination.
*/

import (
	"maps"
	"math"
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

var squares = []int{0, 1, 4, 9, 16, 25, 36, 49, 64, 81}

func isGroup89(n int) bool {
	for n != 89 && n > 1 {
		acc := 0
		for n != 0 {
			acc += squares[n%10]
			n /= 10
		}
		n = acc
	}

	return n == 89
}

func sum(xs []int) int {
	var result int
	for v := range slices.Values(xs) {
		result += v
	}

	return result
}

func compute(limit int) string {
	if e := int(math.Round(math.Log10(float64(limit)))); mylib.Pow(10, e) != limit {
		panic("This implementation works correctly only if the limit is a power of 10")
	}

	nDigits := mylib.NumOfDigits(limit, 10) - 1
	llst := make([][]int, 0)

	for pat := range mylib.CombinationsWithRepetition(squares, nDigits) {
		if isGroup89(sum(pat)) {
			tmp := make([]int, 0)
			for x := range maps.Values(mylib.Frequencies(pat)) {
				tmp = append(tmp, x)
			}
			llst = append(llst, tmp)
		}
	}

	var result int
	var numerator = mylib.Factorial(nDigits)
	for lst := range slices.Values(llst) {
		denominator := 1
		for i := range slices.Values(lst) {
			denominator *= mylib.Factorial(i)
		}
		result += numerator / denominator
	}

	return strconv.FormatInt(int64(result), 10)
}

func Solve() string {
	return compute(10_000_000)
}
