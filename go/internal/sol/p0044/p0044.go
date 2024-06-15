package p0044

import (
	"slices"
	"strconv"

	"pe-solver/internal/mylib"
)

/*
getDivisors(n) returns divisors of n(3n-1) which meet the following requirements:
 - They are less than 'n'.
 - They are congruent to 'n' modulo 3.
Note: 'n' and '3n-1' are relatively prime.
*/

func getDivisors(n int) []int {
	result := make([]int, 0)

	lst := slices.Concat(mylib.PrimeFactorization(n), mylib.PrimeFactorization(3*n-1))
	for _, x := range mylib.PfactorsToDivisors(lst) {
		if x < n && x%3 == n%3 {
			result = append(result, x)
		}
	}

	return result
}

/*
P(d) = P(k) - P(j)
-->
d(3d-1) = k(3k-1) - j(3j-1)
        = (k-j)(3(k+j)-1)
  lhs: d(3d-1)
  rhs: (k-j) * (3(k+j)-1) = r1 * r2 [r1=k-j, r2=3(k+j)-1]
*/

func compute() string {
	pent := func(n int) int {
		return n * (3*n - 1) / 2
	}

	d := 4
	for {
		lhs := d * (3*d - 1)
		for _, r1 := range getDivisors(d) {
			r2 := lhs / r1
			if r2%3 != 2 {
				continue
			}
			tmp := (r2 + 1) / 3
			if (r1+tmp)&1 != 0 {
				continue
			}
			k := (r1 + tmp) / 2
			j := k - r1
			if mylib.IsPentagonal(pent(k) + pent(j)) {
				return strconv.FormatInt(int64(lhs/2), 10)
			}
		}
		d++
	}
}

func Solve() string {
	return compute()
}
